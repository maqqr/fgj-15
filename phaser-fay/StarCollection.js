/*******************************************************************************
 * Misc.
 */

// Workaround for missing functionality in IE 8 and earlier.
if( Object.create === undefined ) {
  Object.create = function( o ) {
    function F(){}
    F.prototype = o;
    return new F();
  };
}

// Insert properties of b in place into a.
function Fay$$objConcat(a,b){
  for (var p in b) if (b.hasOwnProperty(p)){
    a[p] = b[p];
  }
  return a;
}

/*******************************************************************************
 * Thunks.
 */

// Force a thunk (if it is a thunk) until WHNF.
function Fay$$_(thunkish,nocache){
  while (thunkish instanceof Fay$$$) {
    thunkish = thunkish.force(nocache);
  }
  return thunkish;
}

// Apply a function to arguments (see method2 in Fay.hs).
function Fay$$__(){
  var f = arguments[0];
  for (var i = 1, len = arguments.length; i < len; i++) {
    f = (f instanceof Fay$$$? Fay$$_(f) : f)(arguments[i]);
  }
  return f;
}

// Thunk object.
function Fay$$$(value){
  this.forced = false;
  this.value = value;
}

// Force the thunk.
Fay$$$.prototype.force = function(nocache) {
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};


function Fay$$seq(x) {
  return function(y) {
    Fay$$_(x,false);
    return y;
  }
}

function Fay$$seq$36$uncurried(x,y) {
  Fay$$_(x,false);
  return y;
}

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
  this.value = value;
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then(a){
  return function(b){
    return Fay$$bind(a)(function(_){
      return b;
    });
  };
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then$36$uncurried(a,b){
  return Fay$$bind$36$uncurried(a,function(_){ return b; });
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind(m){
  return function(f){
    return new Fay$$$(function(){
      var monad = Fay$$_(m,true);
      return Fay$$_(f)(monad.value);
    });
  };
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind$36$uncurried(m,f){
  return new Fay$$$(function(){
    var monad = Fay$$_(m,true);
    return Fay$$_(f)(monad.value);
  });
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$$_return(a){
  return new Fay$$Monad(a);
}

// Allow the programmer to access thunk forcing directly.
function Fay$$force(thunk){
  return function(type){
    return new Fay$$$(function(){
      Fay$$_(thunk,type);
      return new Fay$$Monad(Fay$$unit);
    })
  }
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$return$36$uncurried(a){
  return new Fay$$Monad(a);
}

// Unit: ().
var Fay$$unit = null;

/*******************************************************************************
 * Serialization.
 * Fay <-> JS. Should be bijective.
 */

// Serialize a Fay object to JS.
function Fay$$fayToJs(type,fayObj){
  var base = type[0];
  var args = type[1];
  var jsObj;
  if(base == "action") {
    // A nullary monadic action. Should become a nullary JS function.
    // Fay () -> function(){ return ... }
    return function(){
      return Fay$$fayToJs(args[0],Fay$$_(fayObj,true).value);
    };

  }
  else if(base == "function") {
    // A proper function.
    return function(){
      var fayFunc = fayObj;
      var return_type = args[args.length-1];
      var len = args.length;
      // If some arguments.
      if (len > 1) {
        // Apply to all the arguments.
        fayFunc = Fay$$_(fayFunc,true);
        // TODO: Perhaps we should throw an error when JS
        // passes more arguments than Haskell accepts.

        // Unserialize the JS values to Fay for the Fay callback.
        if (args == "automatic_function")
        {
          for (var i = 0; i < arguments.length; i++) {
            fayFunc = Fay$$fayToJs(["automatic"], Fay$$_(fayFunc(Fay$$jsToFay(["automatic"],arguments[i])),true));
          }
          return fayFunc;
        }

        for (var i = 0, len = len; i < len - 1 && fayFunc instanceof Function; i++) {
          fayFunc = Fay$$_(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);
        }
        // Finally, serialize the Fay return value back to JS.
        var return_base = return_type[0];
        var return_args = return_type[1];
        // If it's a monadic return value, get the value instead.
        if(return_base == "action") {
          return Fay$$fayToJs(return_args[0],fayFunc.value);
        }
        // Otherwise just serialize the value direct.
        else {
          return Fay$$fayToJs(return_type,fayFunc);
        }
      } else {
        throw new Error("Nullary function?");
      }
    };

  }
  else if(base == "string") {
    return Fay$$fayToJs_string(fayObj);
  }
  else if(base == "list") {
    // Serialize Fay list to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[0],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    return arr;
  }
  else if(base == "tuple") {
    // Serialize Fay tuple to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    var i = 0;
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[i++],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    return arr;
  }
  else if(base == "defined") {
    fayObj = Fay$$_(fayObj);
    return fayObj instanceof Fay.FFI._Undefined
      ? undefined
      : Fay$$fayToJs(args[0],fayObj.slot1);
  }
  else if(base == "nullable") {
    fayObj = Fay$$_(fayObj);
    return fayObj instanceof Fay.FFI._Null
      ? null
      : Fay$$fayToJs(args[0],fayObj.slot1);
  }
  else if(base == "double" || base == "int" || base == "bool") {
    // Bools are unboxed.
    return Fay$$_(fayObj);
  }
  else if(base == "ptr" || base == "unknown")
    return fayObj;
  else if(base == "automatic" && fayObj instanceof Function) {
    return Fay$$fayToJs(["function", "automatic_function"], fayObj);
  }
  else if(base == "automatic" || base == "user") {
    fayObj = Fay$$_(fayObj);

    if(fayObj instanceof Fay$$Cons || fayObj === null){
      // Serialize Fay list to JavaScript array.
      var arr = [];
      while(fayObj instanceof Fay$$Cons) {
        arr.push(Fay$$fayToJs(["automatic"],fayObj.car));
        fayObj = Fay$$_(fayObj.cdr);
      }
      return arr;
    } else {
      var fayToJsFun = fayObj && fayObj.instance && Fay$$fayToJsHash[fayObj.instance];
      return fayToJsFun ? fayToJsFun(type,type[2],fayObj) : fayObj;
    }
  }

  throw new Error("Unhandled Fay->JS translation type: " + base);
}

// Stores the mappings from fay types to js objects.
// This will be populated by compiled modules.
var Fay$$fayToJsHash = {};

// Specialized serializer for string.
function Fay$$fayToJs_string(fayObj){
  // Serialize Fay string to JavaScript string.
  var str = "";
  fayObj = Fay$$_(fayObj);
  while(fayObj instanceof Fay$$Cons) {
    str += Fay$$_(fayObj.car);
    fayObj = Fay$$_(fayObj.cdr);
  }
  return str;
};
function Fay$$jsToFay_string(x){
  return Fay$$list(x)
};

// Special num/bool serializers.
function Fay$$jsToFay_int(x){return x;}
function Fay$$jsToFay_double(x){return x;}
function Fay$$jsToFay_bool(x){return x;}

function Fay$$fayToJs_int(x){return Fay$$_(x);}
function Fay$$fayToJs_double(x){return Fay$$_(x);}
function Fay$$fayToJs_bool(x){return Fay$$_(x);}

// Unserialize an object from JS to Fay.
function Fay$$jsToFay(type,jsObj){
  var base = type[0];
  var args = type[1];
  var fayObj;
  if(base == "action") {
    // Unserialize a "monadic" JavaScript return value into a monadic value.
    return new Fay$$Monad(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "function") {
    // Unserialize a function from JavaScript to a function that Fay can call.
    // So
    //
    //    var f = function(x,y,z){ … }
    //
    // becomes something like:
    //
    //    function(x){
    //      return function(y){
    //        return function(z){
    //          return new Fay$$$(function(){
    //            return Fay$$jsToFay(f(Fay$$fayTojs(x),
    //                                  Fay$$fayTojs(y),
    //                                  Fay$$fayTojs(z))
    //    }}}}};
    var returnType = args[args.length-1];
    var funArgs = args.slice(0,-1);

    if (jsObj.length > 0) {
      var makePartial = function(args){
        return function(arg){
          var i = args.length;
          var fayArg = Fay$$fayToJs(funArgs[i],arg);
          var newArgs = args.concat([fayArg]);
          if(newArgs.length == funArgs.length) {
            return new Fay$$$(function(){
              return Fay$$jsToFay(returnType,jsObj.apply(this,newArgs));
            });
          } else {
            return makePartial(newArgs);
          }
        };
      };
      return makePartial([]);
    }
    else
      return function (arg) {
        return Fay$$jsToFay(["automatic"], jsObj(Fay$$fayToJs(["automatic"], arg)));
      };
  }
  else if(base == "string") {
    // Unserialize a JS string into Fay list (String).
    // This is a special case, when String is explicit in the type signature,
    // with `Automatic' a string would not be decoded.
    return Fay$$list(jsObj);
  }
  else if(base == "list") {
    // Unserialize a JS array into a Fay list ([a]).
    var serializedList = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));
    }
    // Pop it all in a Fay list.
    return Fay$$list(serializedList);
  }
  else if(base == "tuple") {
    // Unserialize a JS array into a Fay tuple ((a,b,c,...)).
    var serializedTuple = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedTuple.push(Fay$$jsToFay(args[i],jsObj[i]));
    }
    // Pop it all in a Fay list.
    return Fay$$list(serializedTuple);
  }
  else if(base == "defined") {
    return jsObj === undefined
      ? new Fay.FFI._Undefined()
      : new Fay.FFI._Defined(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "nullable") {
    return jsObj === null
      ? new Fay.FFI._Null()
      : new Fay.FFI.Nullable(Fay$$jsToFay(args[0],jsObj));
  }
  else if(base == "int") {
    // Int are unboxed, so there's no forcing to do.
    // But we can do validation that the int has no decimal places.
    // E.g. Math.round(x)!=x? throw "NOT AN INTEGER, GET OUT!"
    fayObj = Math.round(jsObj);
    if(fayObj!==jsObj) throw "Argument " + jsObj + " is not an integer!";
    return fayObj;
  }
  else if (base == "double" ||
           base == "bool" ||
           base ==  "ptr" ||
           base ==  "unknown") {
    return jsObj;
  }
  else if(base == "automatic" && jsObj instanceof Function) {
    var type = [["automatic"]];
    for (var i = 0; i < jsObj.length; i++)
      type.push(["automatic"]);
    return Fay$$jsToFay(["function", type], jsObj);
  }
  else if(base == "automatic" && jsObj instanceof Array) {
    var list = null;
    for (var i = jsObj.length - 1; i >= 0; i--) {
      list = new Fay$$Cons(Fay$$jsToFay([base], jsObj[i]), list);
    }
    return list;
  }
  else if(base == "automatic" || base == "user") {
    if (jsObj && jsObj['instance']) {
      var jsToFayFun = Fay$$jsToFayHash[jsObj["instance"]];
      return jsToFayFun ? jsToFayFun(type,type[2],jsObj) : jsObj;
    }
    else
      return jsObj;
  }

  throw new Error("Unhandled JS->Fay translation type: " + base);
}

// Stores the mappings from js objects to fay types.
// This will be populated by compiled modules.
var Fay$$jsToFayHash = {};

/*******************************************************************************
 * Lists.
 */

// Cons object.
function Fay$$Cons(car,cdr){
  this.car = car;
  this.cdr = cdr;
}

// Make a list.
function Fay$$list(xs){
  var out = null;
  for(var i=xs.length-1; i>=0;i--)
    out = new Fay$$Cons(xs[i],out);
  return out;
}

// Built-in list cons.
function Fay$$cons(x){
  return function(y){
    return new Fay$$Cons(x,y);
  };
}

// List index.
// `list' is already forced by the time it's passed to this function.
// `list' cannot be null and `index' cannot be out of bounds.
function Fay$$index(index,list){
  for(var i = 0; i < index; i++) {
    list = Fay$$_(list.cdr);
  }
  return list.car;
}

// List length.
// `list' is already forced by the time it's passed to this function.
function Fay$$listLen(list,max){
  for(var i = 0; list !== null && i < max + 1; i++) {
    list = Fay$$_(list.cdr);
  }
  return i == max;
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) * Fay$$_(y);
    });
  };
}

function Fay$$mult$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) * Fay$$_(y);
  });

}

// Built-in +.
function Fay$$add(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) + Fay$$_(y);
    });
  };
}

// Built-in +.
function Fay$$add$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) + Fay$$_(y);
  });

}

// Built-in -.
function Fay$$sub(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) - Fay$$_(y);
    });
  };
}
// Built-in -.
function Fay$$sub$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) - Fay$$_(y);
  });

}

// Built-in /.
function Fay$$divi(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) / Fay$$_(y);
    });
  };
}

// Built-in /.
function Fay$$divi$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) / Fay$$_(y);
  });

}

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1, lit2) {
  // Simple case
  lit1 = Fay$$_(lit1);
  lit2 = Fay$$_(lit2);
  if (lit1 === lit2) {
    return true;
  }
  // General case
  if (lit1 instanceof Array) {
    if (lit1.length != lit2.length) return false;
    for (var len = lit1.length, i = 0; i < len; i++) {
      if (!Fay$$equal(lit1[i], lit2[i])) return false;
    }
    return true;
  } else if (lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons) {
    do {
      if (!Fay$$equal(lit1.car,lit2.car))
        return false;
      lit1 = Fay$$_(lit1.cdr), lit2 = Fay$$_(lit2.cdr);
      if (lit1 === null || lit2 === null)
        return lit1 === lit2;
    } while (true);
  } else if (typeof lit1 == 'object' && typeof lit2 == 'object' && lit1 && lit2 &&
             lit1.instance === lit2.instance) {
    for(var x in lit1) {
      if(!Fay$$equal(lit1[x],lit2[x]))
        return false;
    }
    return true;
  } else {
    return false;
  }
}

// Built-in ==.
function Fay$$eq(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$equal(x,y);
    });
  };
}

function Fay$$eq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$equal(x,y);
  });

}

// Built-in /=.
function Fay$$neq(x){
  return function(y){
    return new Fay$$$(function(){
      return !(Fay$$equal(x,y));
    });
  };
}

// Built-in /=.
function Fay$$neq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return !(Fay$$equal(x,y));
  });

}

// Built-in >.
function Fay$$gt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) > Fay$$_(y);
    });
  };
}

// Built-in >.
function Fay$$gt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) > Fay$$_(y);
  });

}

// Built-in <.
function Fay$$lt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) < Fay$$_(y);
    });
  };
}


// Built-in <.
function Fay$$lt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) < Fay$$_(y);
  });

}


// Built-in >=.
function Fay$$gte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) >= Fay$$_(y);
    });
  };
}

// Built-in >=.
function Fay$$gte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) >= Fay$$_(y);
  });

}

// Built-in <=.
function Fay$$lte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) <= Fay$$_(y);
    });
  };
}

// Built-in <=.
function Fay$$lte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) <= Fay$$_(y);
  });

}

// Built-in &&.
function Fay$$and(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) && Fay$$_(y);
    });
  };
}

// Built-in &&.
function Fay$$and$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) && Fay$$_(y);
  });
  ;
}

// Built-in ||.
function Fay$$or(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) || Fay$$_(y);
    });
  };
}

// Built-in ||.
function Fay$$or$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) || Fay$$_(y);
  });

}

/*******************************************************************************
 * Mutable references.
 */

// Make a new mutable reference.
function Fay$$Ref(x){
  this.value = x;
}

// Write to the ref.
function Fay$$writeRef(ref,x){
  ref.value = x;
}

// Get the value from the ref.
function Fay$$readRef(ref,x){
  return ref.value;
}

/*******************************************************************************
 * Dates.
 */
function Fay$$date(str){
  return window.Date.parse(str);
}

/*******************************************************************************
 * Data.Var
 */

function Fay$$Ref2(val){
  this.val = val;
}

function Fay$$Sig(){
  this.handlers = [];
}

function Fay$$Var(val){
  this.val = val;
  this.handlers = [];
}

// Helper used by Fay$$setValue and for merging
function Fay$$broadcastInternal(self, val, force){
  var handlers = self.handlers;
  var exceptions = [];
  for(var len = handlers.length, i = 0; i < len; i++) {
    try {
      force(handlers[i][1](val), true);
    } catch (e) {
      exceptions.push(e);
    }
  }
  // Rethrow the encountered exceptions.
  if (exceptions.length > 0) {
    console.error("Encountered " + exceptions.length + " exception(s) while broadcasing a change to ", self);
    for(var len = exceptions.length, i = 0; i < len; i++) {
      (function(exception) {
        window.setTimeout(function() { throw exception; }, 0);
      })(exceptions[i]);
    }
  }
}

function Fay$$setValue(self, val, force){
  if (self instanceof Fay$$Ref2) {
    self.val = val;
  } else if (self instanceof Fay$$Var) {
    self.val = val;
    Fay$$broadcastInternal(self, val, force);
  } else if (self instanceof Fay$$Sig) {
    Fay$$broadcastInternal(self, val, force);
  } else {
    throw "Fay$$setValue given something that's not a Ref2, Var, or Sig"
  }
}

function Fay$$subscribe(self, f){
  var key = {};
  self.handlers.push([key,f]);
  var searchStart = self.handlers.length - 1;
  return function(_){
    for(var i = Math.min(searchStart, self.handlers.length - 1); i >= 0; i--) {
      if(self.handlers[i][0] == key) {
        self.handlers = self.handlers.slice(0,i).concat(self.handlers.slice(i+1));
        return;
      }
    }
    return _; // This variable has to be used, otherwise Closure
              // strips it out and Fay serialization breaks.
  };
}

/*******************************************************************************
 * Application code.
 */
var Data = {};Data.Data = {};var Fay = {};Fay.FFI = {};Fay.FFI._Nullable = function Nullable(slot1){this.slot1 = slot1;};Fay.FFI._Nullable.prototype.instance = "Nullable";Fay.FFI.Nullable = function(slot1){return new Fay$$$(function(){return new Fay.FFI._Nullable(slot1);});};Fay.FFI._Null = function Null(){};Fay.FFI._Null.prototype.instance = "Null";Fay.FFI.Null = new Fay$$$(function(){return new Fay.FFI._Null();});Fay.FFI._Defined = function Defined(slot1){this.slot1 = slot1;};Fay.FFI._Defined.prototype.instance = "Defined";Fay.FFI.Defined = function(slot1){return new Fay$$$(function(){return new Fay.FFI._Defined(slot1);});};Fay.FFI._Undefined = function Undefined(){};Fay.FFI._Undefined.prototype.instance = "Undefined";Fay.FFI.Undefined = new Fay$$$(function(){return new Fay.FFI._Undefined();});Fay$$objConcat(Fay$$fayToJsHash,{"Nullable": function(type,argTypes,_obj){var obj_ = {"instance": "Nullable"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Null": function(type,argTypes,_obj){var obj_ = {"instance": "Null"};return obj_;},"Defined": function(type,argTypes,_obj){var obj_ = {"instance": "Defined"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Undefined": function(type,argTypes,_obj){var obj_ = {"instance": "Undefined"};return obj_;}});Fay$$objConcat(Fay$$jsToFayHash,{"Nullable": function(type,argTypes,obj){return new Fay.FFI._Nullable(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"Null": function(type,argTypes,obj){return new Fay.FFI._Null();},"Defined": function(type,argTypes,obj){return new Fay.FFI._Defined(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"Undefined": function(type,argTypes,obj){return new Fay.FFI._Undefined();}});var Prelude = {};Prelude._Just = function Just(slot1){this.slot1 = slot1;};Prelude._Just.prototype.instance = "Just";Prelude.Just = function(slot1){return new Fay$$$(function(){return new Prelude._Just(slot1);});};Prelude._Nothing = function Nothing(){};Prelude._Nothing.prototype.instance = "Nothing";Prelude.Nothing = new Fay$$$(function(){return new Prelude._Nothing();});Prelude._Left = function Left(slot1){this.slot1 = slot1;};Prelude._Left.prototype.instance = "Left";Prelude.Left = function(slot1){return new Fay$$$(function(){return new Prelude._Left(slot1);});};Prelude._Right = function Right(slot1){this.slot1 = slot1;};Prelude._Right.prototype.instance = "Right";Prelude.Right = function(slot1){return new Fay$$$(function(){return new Prelude._Right(slot1);});};Prelude.maybe = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) instanceof Prelude._Nothing) {var m = $p1;return m;}if (Fay$$_($p3) instanceof Prelude._Just) {var x = Fay$$_($p3).slot1;var f = $p2;return Fay$$_(f)(x);}throw ["unhandled case in maybe",[$p1,$p2,$p3]];});};};};Prelude.$62$$62$$61$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$_(Fay$$bind($p1)($p2));});};};Prelude.$62$$62$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$_(Fay$$then($p1)($p2));});};};Prelude.$_return = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$return(Fay$$fayToJs(["unknown"],$p1))));});};Prelude.fail = new Fay$$$(function(){return Prelude.error;});Prelude.when = function($p1){return function($p2){return new Fay$$$(function(){var m = $p2;var p = $p1;return Fay$$_(p) ? Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Fay$$$_return)(Fay$$unit)) : Fay$$_(Fay$$$_return)(Fay$$unit);});};};Prelude.unless = function($p1){return function($p2){return new Fay$$$(function(){var m = $p2;var p = $p1;return Fay$$_(p) ? Fay$$_(Fay$$$_return)(Fay$$unit) : Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Fay$$$_return)(Fay$$unit));});};};Prelude.forM = function($p1){return function($p2){return new Fay$$$(function(){var fn = $p2;var lst = $p1;return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.sequence))(Fay$$_(Fay$$_(Prelude.map)(fn))(lst));});};};Prelude.forM_ = function($p1){return function($p2){return new Fay$$$(function(){var m = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(m)(x)))(Fay$$_(Fay$$_(Prelude.forM_)(xs))(m));}if (Fay$$_($p1) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in forM_",[$p1,$p2]];});};};Prelude.mapM = function($p1){return function($p2){return new Fay$$$(function(){var lst = $p2;var fn = $p1;return Fay$$_(Fay$$_(Prelude.$36$)(Prelude.sequence))(Fay$$_(Fay$$_(Prelude.map)(fn))(lst));});};};Prelude.mapM_ = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var m = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(m)(x)))(Fay$$_(Fay$$_(Prelude.mapM_)(m))(xs));}if (Fay$$_($p2) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in mapM_",[$p1,$p2]];});};};Prelude.$61$$60$$60$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$bind)(x))(f);});};};Prelude.$_void = function($p1){return new Fay$$$(function(){var f = $p1;return Fay$$_(Fay$$_(Fay$$then)(f))(Fay$$_(Fay$$$_return)(Fay$$unit));});};Prelude.$62$$61$$62$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var x = $p3;var g = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(f)(x)))(g);});};};};Prelude.$60$$61$$60$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var x = $p3;var f = $p2;var g = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(f)(x)))(g);});};};};Prelude.sequence = function($p1){return new Fay$$$(function(){var ms = $p1;return (function(){var k = function($p1){return function($p2){return new Fay$$$(function(){var m$39$ = $p2;var m = $p1;return Fay$$_(Fay$$_(Fay$$bind)(m))(function($p1){var x = $p1;return Fay$$_(Fay$$_(Fay$$bind)(m$39$))(function($p1){var xs = $p1;return Fay$$_(Fay$$$_return)(Fay$$_(Fay$$_(Fay$$cons)(x))(xs));});});});};};return Fay$$_(Fay$$_(Fay$$_(Prelude.foldr)(k))(Fay$$_(Fay$$$_return)(null)))(ms);})();});};Prelude.sequence_ = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var m = $tmp1.car;var ms = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Prelude.sequence_)(ms));}throw ["unhandled case in sequence_",[$p1]];});};Prelude._GT = function GT(){};Prelude._GT.prototype.instance = "GT";Prelude.GT = new Fay$$$(function(){return new Prelude._GT();});Prelude._LT = function LT(){};Prelude._LT.prototype.instance = "LT";Prelude.LT = new Fay$$$(function(){return new Prelude._LT();});Prelude._EQ = function EQ(){};Prelude._EQ.prototype.instance = "EQ";Prelude.EQ = new Fay$$$(function(){return new Prelude._EQ();});Prelude.compare = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(y)) ? Prelude.GT : Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(y)) ? Prelude.LT : Prelude.EQ;});};};Prelude.succ = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$add)(x))(1);});};Prelude.pred = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$sub)(x))(1);});};Prelude.enumFrom = function($p1){return new Fay$$$(function(){var i = $p1;return Fay$$_(Fay$$_(Fay$$cons)(i))(Fay$$_(Prelude.enumFrom)(Fay$$_(Fay$$_(Fay$$add)(i))(1)));});};Prelude.enumFromTo = function($p1){return function($p2){return new Fay$$$(function(){var n = $p2;var i = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(i))(n)) ? null : Fay$$_(Fay$$_(Fay$$cons)(i))(Fay$$_(Fay$$_(Prelude.enumFromTo)(Fay$$_(Fay$$_(Fay$$add)(i))(1)))(n));});};};Prelude.enumFromBy = function($p1){return function($p2){return new Fay$$$(function(){var by = $p2;var fr = $p1;return Fay$$_(Fay$$_(Fay$$cons)(fr))(Fay$$_(Fay$$_(Prelude.enumFromBy)(Fay$$_(Fay$$_(Fay$$add)(fr))(by)))(by));});};};Prelude.enumFromThen = function($p1){return function($p2){return new Fay$$$(function(){var th = $p2;var fr = $p1;return Fay$$_(Fay$$_(Prelude.enumFromBy)(fr))(Fay$$_(Fay$$_(Fay$$sub)(th))(fr));});};};Prelude.enumFromByTo = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var to = $p3;var by = $p2;var fr = $p1;return (function(){var neg = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(neg)(Fay$$_(Fay$$_(Fay$$add)(x))(by)));});};var pos = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(pos)(Fay$$_(Fay$$_(Fay$$add)(x))(by)));});};return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(by))(0)) ? Fay$$_(neg)(fr) : Fay$$_(pos)(fr);})();});};};};Prelude.enumFromThenTo = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var to = $p3;var th = $p2;var fr = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.enumFromByTo)(fr))(Fay$$_(Fay$$_(Fay$$sub)(th))(fr)))(to);});};};};Prelude.fromIntegral = function($p1){return new Fay$$$(function(){return $p1;});};Prelude.fromInteger = function($p1){return new Fay$$$(function(){return $p1;});};Prelude.not = function($p1){return new Fay$$$(function(){var p = $p1;return Fay$$_(p) ? false : true;});};Prelude.otherwise = true;Prelude.show = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_string(JSON.stringify(Fay$$fayToJs(["automatic"],$p1)));});};Prelude.error = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],(function() { throw Fay$$fayToJs_string($p1) })());});};Prelude.$_undefined = new Fay$$$(function(){return Fay$$_(Prelude.error)(Fay$$list("Prelude.undefined"));});Prelude.either = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) instanceof Prelude._Left) {var a = Fay$$_($p3).slot1;var f = $p1;return Fay$$_(f)(a);}if (Fay$$_($p3) instanceof Prelude._Right) {var b = Fay$$_($p3).slot1;var g = $p2;return Fay$$_(g)(b);}throw ["unhandled case in either",[$p1,$p2,$p3]];});};};};Prelude.until = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var x = $p3;var f = $p2;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? x : Fay$$_(Fay$$_(Fay$$_(Prelude.until)(p))(f))(Fay$$_(f)(x));});};};};Prelude.$36$$33$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$seq)(x))(Fay$$_(f)(x));});};};Prelude.$_const = function($p1){return function($p2){return new Fay$$$(function(){var a = $p1;return a;});};};Prelude.id = function($p1){return new Fay$$$(function(){var x = $p1;return x;});};Prelude.$46$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var x = $p3;var g = $p2;var f = $p1;return Fay$$_(f)(Fay$$_(g)(x));});};};};Prelude.$36$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(f)(x);});};};Prelude.flip = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var y = $p3;var x = $p2;var f = $p1;return Fay$$_(Fay$$_(f)(y))(x);});};};};Prelude.curry = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var y = $p3;var x = $p2;var f = $p1;return Fay$$_(f)(Fay$$list([x,y]));});};};};Prelude.uncurry = function($p1){return function($p2){return new Fay$$$(function(){var p = $p2;var f = $p1;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var x = Fay$$index(0,Fay$$_($tmp1));var y = Fay$$index(1,Fay$$_($tmp1));return Fay$$_(Fay$$_(f)(x))(y);}return (function(){ throw (["unhandled case",$tmp1]); })();})(p);});};};Prelude.snd = function($p1){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p1),2)) {var x = Fay$$index(1,Fay$$_($p1));return x;}throw ["unhandled case in snd",[$p1]];});};Prelude.fst = function($p1){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p1),2)) {var x = Fay$$index(0,Fay$$_($p1));return x;}throw ["unhandled case in fst",[$p1]];});};Prelude.div = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude.quot)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y)))(1);} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude.quot)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y)))(1);}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude.quot)(x))(y);});};};Prelude.mod = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude.rem)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y)))(y)))(1);} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude.rem)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y)))(y)))(1);}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude.rem)(x))(y);});};};Prelude.divMod = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var q = Fay$$index(0,Fay$$_($tmp1));var r = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(q))(1),Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(r))(y)))(1)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.quotRem)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y));} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(1)))) {return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var q = Fay$$index(0,Fay$$_($tmp1));var r = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(q))(1),Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$add)(r))(y)))(1)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.quotRem)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y));}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude.quotRem)(x))(y);});};};Prelude.min = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.min(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));});};};Prelude.max = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.max(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));});};};Prelude.recip = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(1))(x);});};Prelude.negate = function($p1){return new Fay$$$(function(){var x = $p1;return (-(Fay$$_(x)));});};Prelude.abs = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(0)) ? Fay$$_(Prelude.negate)(x) : x;});};Prelude.signum = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(0)) ? 1 : Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(x))(0)) ? 0 : (-(1));});};Prelude.pi = new Fay$$$(function(){return Fay$$jsToFay_double(Math.PI);});Prelude.exp = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.exp(Fay$$fayToJs_double($p1)));});};Prelude.sqrt = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.sqrt(Fay$$fayToJs_double($p1)));});};Prelude.log = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.log(Fay$$fayToJs_double($p1)));});};Prelude.$42$$42$ = new Fay$$$(function(){return Prelude.unsafePow;});Prelude.$94$$94$ = new Fay$$$(function(){return Prelude.unsafePow;});Prelude.unsafePow = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.pow(Fay$$_(Fay$$fayToJs(["unknown"],$p1)),Fay$$_(Fay$$fayToJs(["unknown"],$p2))));});};};Prelude.$94$ = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(b))(0))) {return Fay$$_(Prelude.error)(Fay$$list("(^): negative exponent"));} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(b))(0))) {return 1;} else {if (Fay$$_(Fay$$_(Prelude.even)(b))) {return (function(){return new Fay$$$(function(){var x = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude.$94$)(a))(Fay$$_(Fay$$_(Prelude.quot)(b))(2));});return Fay$$_(Fay$$_(Fay$$mult)(x))(x);});})();}}}var b = $p2;var a = $p1;return Fay$$_(Fay$$_(Fay$$mult)(a))(Fay$$_(Fay$$_(Prelude.$94$)(a))(Fay$$_(Fay$$_(Fay$$sub)(b))(1)));});};};Prelude.logBase = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var b = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude.log)(x)))(Fay$$_(Prelude.log)(b));});};};Prelude.sin = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.sin(Fay$$fayToJs_double($p1)));});};Prelude.tan = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.tan(Fay$$fayToJs_double($p1)));});};Prelude.cos = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.cos(Fay$$fayToJs_double($p1)));});};Prelude.asin = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.asin(Fay$$fayToJs_double($p1)));});};Prelude.atan = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.atan(Fay$$fayToJs_double($p1)));});};Prelude.acos = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.acos(Fay$$fayToJs_double($p1)));});};Prelude.sinh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Prelude.exp)(x)))(Fay$$_(Prelude.exp)((-(Fay$$_(x)))))))(2);});};Prelude.tanh = function($p1){return new Fay$$$(function(){var x = $p1;return (function(){return new Fay$$$(function(){var a = new Fay$$$(function(){return Fay$$_(Prelude.exp)(x);});var b = new Fay$$$(function(){return Fay$$_(Prelude.exp)((-(Fay$$_(x))));});return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(a))(b)))(Fay$$_(Fay$$_(Fay$$add)(a))(b));});})();});};Prelude.cosh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Prelude.exp)(x)))(Fay$$_(Prelude.exp)((-(Fay$$_(x)))))))(2);});};Prelude.asinh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude.log)(Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Prelude.sqrt)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude.$42$$42$)(x))(2)))(1))));});};Prelude.atanh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude.log)(Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(1))(x)))(Fay$$_(Fay$$_(Fay$$sub)(1))(x)))))(2);});};Prelude.acosh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude.log)(Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Prelude.sqrt)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude.$42$$42$)(x))(2)))(1))));});};Prelude.properFraction = function($p1){return new Fay$$$(function(){var x = $p1;return (function(){return new Fay$$$(function(){var a = new Fay$$$(function(){return Fay$$_(Prelude.truncate)(x);});return Fay$$list([a,Fay$$_(Fay$$_(Fay$$sub)(x))(Fay$$_(Prelude.fromIntegral)(a))]);});})();});};Prelude.truncate = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(0)) ? Fay$$_(Prelude.ceiling)(x) : Fay$$_(Prelude.floor)(x);});};Prelude.round = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.round(Fay$$fayToJs_double($p1)));});};Prelude.ceiling = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.ceil(Fay$$fayToJs_double($p1)));});};Prelude.floor = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.floor(Fay$$fayToJs_double($p1)));});};Prelude.subtract = new Fay$$$(function(){return Fay$$_(Prelude.flip)(Fay$$sub);});Prelude.even = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$eq)(Fay$$_(Fay$$_(Prelude.rem)(x))(2)))(0);});};Prelude.odd = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude.not)(Fay$$_(Prelude.even)(x));});};Prelude.gcd = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;return (function(){var go = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === 0) {var x = $p1;return x;}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(go)(y))(Fay$$_(Fay$$_(Prelude.rem)(x))(y));});};};return Fay$$_(Fay$$_(go)(Fay$$_(Prelude.abs)(a)))(Fay$$_(Prelude.abs)(b));})();});};};Prelude.quot = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(y))(0)) ? Fay$$_(Prelude.error)(Fay$$list("Division by zero")) : Fay$$_(Fay$$_(Prelude.quot$39$)(x))(y);});};};Prelude.quot$39$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay_int(~~(Fay$$fayToJs_int($p1)/Fay$$fayToJs_int($p2)));});};};Prelude.quotRem = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$list([Fay$$_(Fay$$_(Prelude.quot)(x))(y),Fay$$_(Fay$$_(Prelude.rem)(x))(y)]);});};};Prelude.rem = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(y))(0)) ? Fay$$_(Prelude.error)(Fay$$list("Division by zero")) : Fay$$_(Fay$$_(Prelude.rem$39$)(x))(y);});};};Prelude.rem$39$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay_int(Fay$$fayToJs_int($p1) % Fay$$fayToJs_int($p2));});};};Prelude.lcm = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === 0) {return 0;}if (Fay$$_($p1) === 0) {return 0;}var b = $p2;var a = $p1;return Fay$$_(Prelude.abs)(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Prelude.quot)(a))(Fay$$_(Fay$$_(Prelude.gcd)(a))(b))))(b));});};};Prelude.find = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Prelude.Just)(x) : Fay$$_(Fay$$_(Prelude.find)(p))(xs);}if (Fay$$_($p2) === null) {return Prelude.Nothing;}throw ["unhandled case in find",[$p1,$p2]];});};};Prelude.filter = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.filter)(p))(xs)) : Fay$$_(Fay$$_(Prelude.filter)(p))(xs);}if (Fay$$_($p2) === null) {return null;}throw ["unhandled case in filter",[$p1,$p2]];});};};Prelude.$_null = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return true;}return false;});};Prelude.map = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(f)(x)))(Fay$$_(Fay$$_(Prelude.map)(f))(xs));}throw ["unhandled case in map",[$p1,$p2]];});};};Prelude.nub = function($p1){return new Fay$$$(function(){var ls = $p1;return Fay$$_(Fay$$_(Prelude.nub$39$)(ls))(null);});};Prelude.nub$39$ = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var ls = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$_(Prelude.elem)(x))(ls)) ? Fay$$_(Fay$$_(Prelude.nub$39$)(xs))(ls) : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.nub$39$)(xs))(Fay$$_(Fay$$_(Fay$$cons)(x))(ls)));}throw ["unhandled case in nub'",[$p1,$p2]];});};};Prelude.elem = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var y = $tmp1.car;var ys = $tmp1.cdr;var x = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(Fay$$_(Fay$$eq)(x))(y)))(Fay$$_(Fay$$_(Prelude.elem)(x))(ys));}if (Fay$$_($p2) === null) {return false;}throw ["unhandled case in elem",[$p1,$p2]];});};};Prelude.notElem = function($p1){return function($p2){return new Fay$$$(function(){var ys = $p2;var x = $p1;return Fay$$_(Prelude.not)(Fay$$_(Fay$$_(Prelude.elem)(x))(ys));});};};Prelude.sort = new Fay$$$(function(){return Fay$$_(Prelude.sortBy)(Prelude.compare);});Prelude.sortBy = function($p1){return new Fay$$$(function(){var cmp = $p1;return Fay$$_(Fay$$_(Prelude.foldr)(Fay$$_(Prelude.insertBy)(cmp)))(null);});};Prelude.insertBy = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var x = $p2;return Fay$$list([x]);}var ys = $p3;var x = $p2;var cmp = $p1;return (function($tmp1){if (Fay$$_($tmp1) === null) {return Fay$$list([x]);}var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var y = $tmp2.car;var ys$39$ = $tmp2.cdr;return (function($tmp2){if (Fay$$_($tmp2) instanceof Prelude._GT) {return Fay$$_(Fay$$_(Fay$$cons)(y))(Fay$$_(Fay$$_(Fay$$_(Prelude.insertBy)(cmp))(x))(ys$39$));}return Fay$$_(Fay$$_(Fay$$cons)(x))(ys);})(Fay$$_(Fay$$_(cmp)(x))(y));}return (function(){ throw (["unhandled case",$tmp1]); })();})(ys);});};};};Prelude.conc = function($p1){return function($p2){return new Fay$$$(function(){var ys = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.conc)(xs))(ys));}var ys = $p2;if (Fay$$_($p1) === null) {return ys;}throw ["unhandled case in conc",[$p1,$p2]];});};};Prelude.concat = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude.foldr)(Prelude.conc))(null);});Prelude.concatMap = function($p1){return new Fay$$$(function(){var f = $p1;return Fay$$_(Fay$$_(Prelude.foldr)(Fay$$_(Fay$$_(Prelude.$46$)(Prelude.$43$$43$))(f)))(null);});};Prelude.foldr = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return z;}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(f)(x))(Fay$$_(Fay$$_(Fay$$_(Prelude.foldr)(f))(z))(xs));}throw ["unhandled case in foldr",[$p1,$p2,$p3]];});};};};Prelude.foldr1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p2),1)) {var x = Fay$$index(0,Fay$$_($p2));return x;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(f)(x))(Fay$$_(Fay$$_(Prelude.foldr1)(f))(xs));}if (Fay$$_($p2) === null) {return Fay$$_(Prelude.error)(Fay$$list("foldr1: empty list"));}throw ["unhandled case in foldr1",[$p1,$p2]];});};};Prelude.foldl = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return z;}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(f))(Fay$$_(Fay$$_(f)(z))(x)))(xs);}throw ["unhandled case in foldl",[$p1,$p2,$p3]];});};};};Prelude.foldl1 = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(f))(x))(xs);}if (Fay$$_($p2) === null) {return Fay$$_(Prelude.error)(Fay$$list("foldl1: empty list"));}throw ["unhandled case in foldl1",[$p1,$p2]];});};};Prelude.$43$$43$ = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude.conc)(x))(y);});};};Prelude.$33$$33$ = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;return (function(){var go = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("(!!): index too large"));}if (Fay$$_($p2) === 0) {var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;return h;}}var n = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return Fay$$_(Fay$$_(go)(t))(Fay$$_(Fay$$_(Fay$$sub)(n))(1));}throw ["unhandled case in go",[$p1,$p2]];});};};return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(b))(0)) ? Fay$$_(Prelude.error)(Fay$$list("(!!): negative index")) : Fay$$_(Fay$$_(go)(a))(b);})();});};};Prelude.head = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("head: empty list"));}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;return h;}throw ["unhandled case in head",[$p1]];});};Prelude.tail = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("tail: empty list"));}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return t;}throw ["unhandled case in tail",[$p1]];});};Prelude.init = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("init: empty list"));}if (Fay$$listLen(Fay$$_($p1),1)) {var a = Fay$$index(0,Fay$$_($p1));return null;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;var t = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(h))(Fay$$_(Prelude.init)(t));}throw ["unhandled case in init",[$p1]];});};Prelude.last = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("last: empty list"));}if (Fay$$listLen(Fay$$_($p1),1)) {var a = Fay$$index(0,Fay$$_($p1));return a;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return Fay$$_(Prelude.last)(t);}throw ["unhandled case in last",[$p1]];});};Prelude.iterate = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.iterate)(f))(Fay$$_(f)(x)));});};};Prelude.repeat = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Prelude.repeat)(x));});};Prelude.replicate = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === 0) {return null;}var x = $p2;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.replicate)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(x));});};};Prelude.cycle = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("cycle: empty list"));}var xs = $p1;return (function(){var xs$39$ = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude.$43$$43$)(xs))(xs$39$);});return xs$39$;})();});};Prelude.take = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === 0) {return null;}if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.take)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs));}throw ["unhandled case in take",[$p1,$p2]];});};};Prelude.drop = function($p1){return function($p2){return new Fay$$$(function(){var xs = $p2;if (Fay$$_($p1) === 0) {return xs;}if (Fay$$_($p2) === null) {return null;}var xss = $p2;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? xss : Fay$$_(Fay$$_(Prelude.drop)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs);}throw ["unhandled case in drop",[$p1,$p2]];});};};Prelude.splitAt = function($p1){return function($p2){return new Fay$$$(function(){var xs = $p2;if (Fay$$_($p1) === 0) {return Fay$$list([null,xs]);}if (Fay$$_($p2) === null) {return Fay$$list([null,null]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]) : (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.splitAt)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs));}throw ["unhandled case in splitAt",[$p1,$p2]];});};};Prelude.takeWhile = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.takeWhile)(p))(xs)) : null;}throw ["unhandled case in takeWhile",[$p1,$p2]];});};};Prelude.dropWhile = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Prelude.dropWhile)(p))(xs) : Fay$$_(Fay$$_(Fay$$cons)(x))(xs);}throw ["unhandled case in dropWhile",[$p1,$p2]];});};};Prelude.span = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return Fay$$list([null,null]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.span)(p))(xs)) : Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]);}throw ["unhandled case in span",[$p1,$p2]];});};};Prelude.$_break = function($p1){return new Fay$$$(function(){var p = $p1;return Fay$$_(Prelude.span)(Fay$$_(Fay$$_(Prelude.$46$)(Prelude.not))(p));});};Prelude.zipWith = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(a))(b)))(Fay$$_(Fay$$_(Fay$$_(Prelude.zipWith)(f))(as))(bs));}}return null;});};};};Prelude.zipWith3 = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var $tmp1 = Fay$$_($p4);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$_(f)(a))(b))(c)))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Prelude.zipWith3)(f))(as))(bs))(cs));}}}return null;});};};};};Prelude.zip = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b])))(Fay$$_(Fay$$_(Prelude.zip)(as))(bs));}}return null;});};};Prelude.zip3 = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b,c])))(Fay$$_(Fay$$_(Fay$$_(Prelude.zip3)(as))(bs))(cs));}}}return null;});};};};Prelude.unzip = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),2)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var ps = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var xs = Fay$$index(0,Fay$$_($tmp1));var ys = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Prelude.unzip)(ps));}}if (Fay$$_($p1) === null) {return Fay$$list([null,null]);}throw ["unhandled case in unzip",[$p1]];});};Prelude.unzip3 = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),3)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var z = Fay$$index(2,Fay$$_($tmp1.car));var ps = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),3)) {var xs = Fay$$index(0,Fay$$_($tmp1));var ys = Fay$$index(1,Fay$$_($tmp1));var zs = Fay$$index(2,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys),Fay$$_(Fay$$_(Fay$$cons)(z))(zs)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Prelude.unzip3)(ps));}}if (Fay$$_($p1) === null) {return Fay$$list([null,null,null]);}throw ["unhandled case in unzip3",[$p1]];});};Prelude.lines = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var s = $p1;return (function(){var isLineBreak = function($p1){return new Fay$$$(function(){var c = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(Fay$$_(Fay$$eq)(c))("\r")))(Fay$$_(Fay$$_(Fay$$eq)(c))("\n"));});};return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));if (Fay$$_(Fay$$index(1,Fay$$_($tmp1))) === null) {return Fay$$list([a]);}}if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var $tmp2 = Fay$$_(Fay$$index(1,Fay$$_($tmp1)));if ($tmp2 instanceof Fay$$Cons) {var cs = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Prelude.lines)(cs));}}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.$_break)(isLineBreak))(s));})();});};Prelude.unlines = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var l = $tmp1.car;var ls = $tmp1.cdr;return Fay$$_(Fay$$_(Prelude.$43$$43$)(l))(Fay$$_(Fay$$_(Fay$$cons)("\n"))(Fay$$_(Prelude.unlines)(ls)));}throw ["unhandled case in unlines",[$p1]];});};Prelude.words = function($p1){return new Fay$$$(function(){var str = $p1;return (function(){var words$39$ = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var s = $p1;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Prelude.words)(b));}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude.$_break)(isSpace))(s));});};var isSpace = function($p1){return new Fay$$$(function(){var c = $p1;return Fay$$_(Fay$$_(Prelude.elem)(c))(Fay$$list(" \t\r\n\u000c\u000b"));});};return Fay$$_(words$39$)(Fay$$_(Fay$$_(Prelude.dropWhile)(isSpace))(str));})();});};Prelude.unwords = new Fay$$$(function(){return Fay$$_(Prelude.intercalate)(Fay$$list(" "));});Prelude.and = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return true;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$and)(x))(Fay$$_(Prelude.and)(xs));}throw ["unhandled case in and",[$p1]];});};Prelude.or = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return false;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$or)(x))(Fay$$_(Prelude.or)(xs));}throw ["unhandled case in or",[$p1]];});};Prelude.any = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return false;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(p)(x)))(Fay$$_(Fay$$_(Prelude.any)(p))(xs));}throw ["unhandled case in any",[$p1,$p2]];});};};Prelude.all = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return true;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(Fay$$and)(Fay$$_(p)(x)))(Fay$$_(Fay$$_(Prelude.all)(p))(xs));}throw ["unhandled case in all",[$p1,$p2]];});};};Prelude.intersperse = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.prependToAll)(sep))(xs));}throw ["unhandled case in intersperse",[$p1,$p2]];});};};Prelude.prependToAll = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return Fay$$_(Fay$$_(Fay$$cons)(sep))(Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude.prependToAll)(sep))(xs)));}throw ["unhandled case in prependToAll",[$p1,$p2]];});};};Prelude.intercalate = function($p1){return function($p2){return new Fay$$$(function(){var xss = $p2;var xs = $p1;return Fay$$_(Prelude.concat)(Fay$$_(Fay$$_(Prelude.intersperse)(xs))(xss));});};};Prelude.maximum = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("maximum: empty list"));}var xs = $p1;return Fay$$_(Fay$$_(Prelude.foldl1)(Prelude.max))(xs);});};Prelude.minimum = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude.error)(Fay$$list("minimum: empty list"));}var xs = $p1;return Fay$$_(Fay$$_(Prelude.foldl1)(Prelude.min))(xs);});};Prelude.product = function($p1){return new Fay$$$(function(){var xs = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(Fay$$mult))(1))(xs);});};Prelude.sum = function($p1){return new Fay$$$(function(){var xs = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.foldl)(Fay$$add))(0))(xs);});};Prelude.scanl = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var l = $p3;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(z))((function($tmp1){if (Fay$$_($tmp1) === null) {return null;}var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var x = $tmp2.car;var xs = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$_(Prelude.scanl)(f))(Fay$$_(Fay$$_(f)(z))(x)))(xs);}return (function(){ throw (["unhandled case",$tmp1]); })();})(l));});};};};Prelude.scanl1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude.scanl)(f))(x))(xs);}throw ["unhandled case in scanl1",[$p1,$p2]];});};};Prelude.scanr = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return Fay$$list([z]);}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return (function($tmp1){var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var h = $tmp2.car;var t = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));}return Prelude.$_undefined;})(Fay$$_(Fay$$_(Fay$$_(Prelude.scanr)(f))(z))(xs));}throw ["unhandled case in scanr",[$p1,$p2,$p3]];});};};};Prelude.scanr1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}if (Fay$$listLen(Fay$$_($p2),1)) {var x = Fay$$index(0,Fay$$_($p2));return Fay$$list([x]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return (function($tmp1){var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var h = $tmp2.car;var t = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));}return Prelude.$_undefined;})(Fay$$_(Fay$$_(Prelude.scanr1)(f))(xs));}throw ["unhandled case in scanr1",[$p1,$p2]];});};};Prelude.lookup = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {var _key = $p1;return Prelude.Nothing;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),2)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var xys = $tmp1.cdr;var key = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(key))(x)) ? Fay$$_(Prelude.Just)(y) : Fay$$_(Fay$$_(Prelude.lookup)(key))(xys);}}throw ["unhandled case in lookup",[$p1,$p2]];});};};Prelude.length = function($p1){return new Fay$$$(function(){var xs = $p1;return Fay$$_(Fay$$_(Prelude.length$39$)(0))(xs);});};Prelude.length$39$ = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;var acc = $p1;return Fay$$_(Fay$$_(Prelude.length$39$)(Fay$$_(Fay$$_(Fay$$add)(acc))(1)))(xs);}var acc = $p1;return acc;});};};Prelude.reverse = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Prelude.$43$$43$)(Fay$$_(Prelude.reverse)(xs)))(Fay$$list([x]));}if (Fay$$_($p1) === null) {return null;}throw ["unhandled case in reverse",[$p1]];});};Prelude.print = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs(["automatic"],$p1))));});};Prelude.putStrLn = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs_string($p1))));});};Prelude.ifThenElse = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var b = $p3;var a = $p2;var p = $p1;return Fay$$_(p) ? a : b;});};};};Fay$$objConcat(Fay$$fayToJsHash,{"Just": function(type,argTypes,_obj){var obj_ = {"instance": "Just"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Nothing": function(type,argTypes,_obj){var obj_ = {"instance": "Nothing"};return obj_;},"Left": function(type,argTypes,_obj){var obj_ = {"instance": "Left"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"Right": function(type,argTypes,_obj){var obj_ = {"instance": "Right"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;},"GT": function(type,argTypes,_obj){var obj_ = {"instance": "GT"};return obj_;},"LT": function(type,argTypes,_obj){var obj_ = {"instance": "LT"};return obj_;},"EQ": function(type,argTypes,_obj){var obj_ = {"instance": "EQ"};return obj_;}});Fay$$objConcat(Fay$$jsToFayHash,{"Just": function(type,argTypes,obj){return new Prelude._Just(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"Nothing": function(type,argTypes,obj){return new Prelude._Nothing();},"Left": function(type,argTypes,obj){return new Prelude._Left(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"Right": function(type,argTypes,obj){return new Prelude._Right(Fay$$jsToFay(argTypes && (argTypes)[1] ? (argTypes)[1] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));},"GT": function(type,argTypes,obj){return new Prelude._GT();},"LT": function(type,argTypes,obj){return new Prelude._LT();},"EQ": function(type,argTypes,obj){return new Prelude._EQ();}});Data.Function = {};Data.Function.on = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var y = $p4;var x = $p3;var g = $p2;var f = $p1;return Fay$$_(Fay$$_(f)(Fay$$_(g)(x)))(Fay$$_(g)(y));});};};};};Data.Function.fmap = function($p1){return function($p2){return new Fay$$$(function(){var a = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$bind)(a))(Fay$$_(Fay$$_(Prelude.$46$)(Fay$$$_return))(f));});};};Data.Function.ap = function($p1){return function($p2){return new Fay$$$(function(){var g = $p2;var m = $p1;return Fay$$_(Fay$$_(Fay$$bind)(m))(function($p1){var f = $p1;return Fay$$_(Fay$$_(Fay$$bind)(g))(function($p1){var x = $p1;return Fay$$_(Fay$$$_return)(Fay$$_(f)(x));});});});};};Data.Function.$60$$42$$62$ = new Fay$$$(function(){return Data.Function.ap;});var FFI = {};var DOM = {};DOM.setBodyHtml = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],document.body.innerHTML = Fay$$fayToJs_string($p1)));});};DOM.addWindowEvent = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],window.addEventListener(Fay$$fayToJs_string($p1), Fay$$fayToJs(["function",[["user","Event",[]],["action",[["unknown"]]]]],$p2))));});};};var Utils = {};Utils.whenM = function($p1){return function($p2){return new Fay$$$(function(){var act = $p2;var cond = $p1;return Fay$$_(Fay$$_(Fay$$bind)(cond))(Fay$$_(Fay$$_(Prelude.flip)(Prelude.when))(act));});};};Utils.unlessM = function($p1){return function($p2){return new Fay$$$(function(){var act = $p2;var cond = $p1;return Fay$$_(Fay$$_(Fay$$bind)(cond))(Fay$$_(Fay$$_(Prelude.flip)(Prelude.unless))(act));});};};Utils.liftM = function($p1){return function($p2){return new Fay$$$(function(){var a = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$bind)(a))(Fay$$_(Fay$$_(Prelude.$46$)(Fay$$$_return))(f));});};};Utils.$124$$62$ = function($p1){return function($p2){return new Fay$$$(function(){var f = $p2;var x = $p1;return Fay$$_(f)(x);});};};var PhaserHS = {};PhaserHS.random = new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_double(Math.random()));});PhaserHS.newGame = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Game",[]],wrNewGame(Fay$$fayToJs_int($p1),Fay$$fayToJs_int($p2))));});};};PhaserHS.setSmoothing = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Game",[]],$p1).stage.smoothed=Fay$$fayToJs_bool($p2)));});};};PhaserHS.loadImage = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Game",[]],$p1).load.image(Fay$$fayToJs_string($p2), Fay$$fayToJs_string($p3))));});};};};PhaserHS.loadSpriteSheet = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],wrLoadSpriteSheet(Fay$$fayToJs(["user","Game",[]],$p1),Fay$$fayToJs_string($p2),Fay$$fayToJs_string($p3),Fay$$fayToJs(["tuple",[["int"],["int"]]],$p4))));});};};};};PhaserHS.loadBitmapFont = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Game",[]],$p1).load.bitmapFont(Fay$$fayToJs_string($p2), Fay$$fayToJs_string($p3), Fay$$fayToJs_string($p4))));});};};};};PhaserHS.newState = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","State",[["unknown"]]],wrNewState(Fay$$fayToJs(["user","Game",[]],$p1),Fay$$fayToJs_string($p2),Fay$$fayToJs(["function",[["user","Game",[]],["action",[["unknown"]]]]],$p3),Fay$$fayToJs(["function",[["user","Game",[]],["action",[["unknown"]]]]],$p4),Fay$$fayToJs(["function",[["user","Game",[]],["user","State",[["unknown"]]],["action",[["unknown"]]]]],$p5))));});};};};};};PhaserHS.changeState = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Game",[]],$p1).state.start(Fay$$fayToJs_string($p2))));});};};PhaserHS.getCurrentState = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","State",[["unknown"]]],Fay$$fayToJs(["user","Game",[]],$p1).state.getCurrentState());});};PhaserHS.get = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","State",[["unknown"]]],$p1).internalState));});};PhaserHS.set = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","State",[["unknown"]]],$p1).internalState=Fay$$fayToJs(["unknown"],$p2)));});};};PhaserHS.modify = function($p1){return function($p2){return new Fay$$$(function(){var func = $p2;var s = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(PhaserHS.get)(s)))(Fay$$_(Fay$$_(Prelude.$46$)(Fay$$$_return))(func))))(Fay$$_(PhaserHS.set)(s));});};};PhaserHS.newText = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","BitmapText",[]],wrNewText(Fay$$fayToJs(["user","Game",[]],$p1),Fay$$fayToJs_string($p2),Fay$$fayToJs_double($p3),Fay$$fayToJs(["tuple",[["double"],["double"]]],$p4),Fay$$fayToJs_string($p5))));});};};};};};PhaserHS.setText = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","BitmapText",[]],$p1).setText(Fay$$fayToJs_string($p2))));});};};PhaserHS.newSprite = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Sprite",[]],wrNewSprite(Fay$$fayToJs(["user","Game",[]],$p1),Fay$$fayToJs_string($p2),Fay$$fayToJs(["tuple",[["double"],["double"]]],$p3))));});};};};PhaserHS.anchor = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Vector",[]],Fay$$fayToJs(["user","Sprite",[]],$p1).anchor);});};PhaserHS.position = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Vector",[]],Fay$$fayToJs(["user","Sprite",[]],$p1).position);});};PhaserHS.addAnimation = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Sprite",[]],$p1).animations.add(Fay$$fayToJs_string($p2), Fay$$fayToJs(["list",[["int"]]],$p3), Fay$$fayToJs_int($p4), Fay$$fayToJs_bool($p5))));});};};};};};PhaserHS.playAnimation = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Sprite",[]],$p1).animations.play(Fay$$fayToJs_string($p2))));});};};PhaserHS.stopAnimation = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Sprite",[]],$p1).animations.stop()));});};PhaserHS.setFrame = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Sprite",[]],$p1).frame=Fay$$fayToJs_int($p2)));});};};PhaserHS.kill = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],$p1.kill()));});};PhaserHS.destroy = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],$p1.destroy()));});};PhaserHS.scale = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Vector",[]],Fay$$fayToJs(["user","Sprite",[]],$p1).scale);});};PhaserHS.setWidth = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Sprite",[]],$p1).width=Fay$$fayToJs_double($p2)));});};};PhaserHS.setHeight = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Sprite",[]],$p1).height=Fay$$fayToJs_double($p2)));});};};PhaserHS.newGroup = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Group",[]],Fay$$fayToJs(["user","Game",[]],$p1).add.group()));});};PhaserHS.enableBody = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Group",[]],$p1).enableBody=Fay$$fayToJs_bool($p2)));});};};PhaserHS.create = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Sprite",[]],wrCreate(Fay$$fayToJs(["user","Group",[]],$p1),Fay$$fayToJs_string($p2),Fay$$fayToJs(["tuple",[["double"],["double"]]],$p3))));});};};};PhaserHS.world = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","World",[]],Fay$$fayToJs(["user","Game",[]],$p1).world);});};PhaserHS.centerX = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Fay$$fayToJs(["user","World",[]],$p1).centerX);});};PhaserHS.centerY = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Fay$$fayToJs(["user","World",[]],$p1).centerY);});};PhaserHS.width = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Fay$$fayToJs(["user","World",[]],$p1).width);});};PhaserHS.height = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Fay$$fayToJs(["user","World",[]],$p1).height);});};PhaserHS.arcadePhysics = new Fay$$$(function(){return Fay$$jsToFay(["user","PhysicsType",[]],Phaser.Physics.ARCADE);});PhaserHS.p2Physics = new Fay$$$(function(){return Fay$$jsToFay(["user","PhysicsType",[]],Phaser.Physics.P2);});PhaserHS.ninjaPhysics = new Fay$$$(function(){return Fay$$jsToFay(["user","PhysicsType",[]],Phaser.Physics.NINJA);});PhaserHS.startPhysics = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Physics",[]],wrStartPhysics(Fay$$fayToJs(["user","Game",[]],$p1),Fay$$fayToJs(["user","PhysicsType",[]],$p2))));});};};PhaserHS.collide = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_bool(wrCollide(Fay$$fayToJs(["user","Physics",[]],$p1),$p2,$p3)));});};};};PhaserHS.overlap = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay_bool(wrOverlap(Fay$$fayToJs(["user","Physics",[]],$p1),$p2,$p3,Fay$$fayToJs(["function",[["user","Sprite",[]],["user","Sprite",[]],["action",[["unknown"]]]]],$p4))));});};};};};PhaserHS.enable = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Physics",[]],$p1).enable(Fay$$fayToJs(["user","Sprite",[]],$p2))));});};};PhaserHS.body = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Body",[]],Fay$$fayToJs(["user","Sprite",[]],$p1).body);});};PhaserHS.touchingDown = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_bool(Fay$$fayToJs(["user","Body",[]],$p1).touching.down);});};PhaserHS.setImmovable = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Body",[]],$p1).immovable=Fay$$fayToJs_bool($p2)));});};};PhaserHS.gravity = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Vector",[]],Fay$$fayToJs(["user","Body",[]],$p1).gravity);});};PhaserHS.velocity = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Vector",[]],Fay$$fayToJs(["user","Body",[]],$p1).velocity);});};PhaserHS.bounce = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","Vector",[]],Fay$$fayToJs(["user","Body",[]],$p1).bounce);});};PhaserHS.getX = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Fay$$fayToJs(["user","Vector",[]],$p1).x);});};PhaserHS.getY = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Fay$$fayToJs(["user","Vector",[]],$p1).y);});};PhaserHS.setX = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Vector",[]],$p2).x=Fay$$fayToJs_double($p1)));});};};PhaserHS.setY = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Vector",[]],$p2).y=Fay$$fayToJs_double($p1)));});};};PhaserHS.vectorToTuple = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["tuple",[["double"],["double"]]],[Fay$$fayToJs(["user","Vector",[]],$p1).x, Fay$$fayToJs(["user","Vector",[]],$p1).y]);});};PhaserHS.setTo = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],wrSetTo(Fay$$fayToJs(["tuple",[["double"],["double"]]],$p1),Fay$$fayToJs(["user","Vector",[]],$p2))));});};};PhaserHS.collideWorldBounds = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Body",[]],$p1).collideWorldBounds=Fay$$fayToJs_bool($p2)));});};};PhaserHS.startGamePad = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Game",[]],$p1).input.gamepad.start()));});};PhaserHS.pad1 = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","GamePad",[]],Fay$$fayToJs(["user","Game",[]],$p1).input.gamepad.pad1);});};PhaserHS.pad2 = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["user","GamePad",[]],Fay$$fayToJs(["user","Game",[]],$p1).input.gamepad.pad2);});};PhaserHS.getGamePadInput = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","GamePadInput",[]],wrGetGamePadInput(Fay$$fayToJs(["user","Game",[]],$p1),Fay$$fayToJs_int($p2))));});};};PhaserHS.padUp = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_bool(Fay$$fayToJs(["user","GamePadInput",[]],$p1).up);});};PhaserHS.padDown = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_bool(Fay$$fayToJs(["user","GamePadInput",[]],$p1).down);});};PhaserHS.padLeft = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_bool(Fay$$fayToJs(["user","GamePadInput",[]],$p1).left);});};PhaserHS.padRight = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_bool(Fay$$fayToJs(["user","GamePadInput",[]],$p1).right);});};PhaserHS.padA = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_bool(Fay$$fayToJs(["user","GamePadInput",[]],$p1).a);});};PhaserHS.padB = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_bool(Fay$$fayToJs(["user","GamePadInput",[]],$p1).b);});};PhaserHS.seconds = new Fay$$$(function(){return Fay$$jsToFay_double(Phaser.Timer.SECOND);});PhaserHS.singleShot = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Game",[]],$p1).time.events.add(Fay$$fayToJs_double($p2), Fay$$fayToJs(["action",[["unknown"]]],$p3), null)));});};};};PhaserHS.repeatTimer = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Game",[]],$p1).time.events.repeat(Fay$$fayToJs_double($p2), Fay$$fayToJs_int($p3), Fay$$fayToJs(["action",[["unknown"]]],$p4), null)));});};};};};PhaserHS.newEmitter = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Emitter",[]],wrNewEmitter(Fay$$fayToJs(["user","Game",[]],$p1),Fay$$fayToJs(["tuple",[["double"],["double"]]],$p2),Fay$$fayToJs_int($p3),Fay$$fayToJs(["list",[["string"]]],$p4))));});};};};};PhaserHS.setEmitterPos = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],wrSetEmitterPos(Fay$$fayToJs(["user","Emitter",[]],$p1),Fay$$fayToJs(["tuple",[["double"],["double"]]],$p2))));});};};PhaserHS.emitterBurst = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Emitter",[]],$p1).start(true, Fay$$fayToJs_double($p2), null, Fay$$fayToJs_int($p3))));});};};};PhaserHS.setParticleMaxSpeed = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],wrSetParticleMaxSpeed(Fay$$fayToJs(["user","Emitter",[]],$p1),Fay$$fayToJs(["tuple",[["double"],["double"]]],$p2))));});};};PhaserHS.setParticleMinSpeed = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],wrSetParticleMinSpeed(Fay$$fayToJs(["user","Emitter",[]],$p1),Fay$$fayToJs(["tuple",[["double"],["double"]]],$p2))));});};};var Arrows = {};Arrows.first = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p2),2)) {var x = Fay$$index(0,Fay$$_($p2));var y = Fay$$index(1,Fay$$_($p2));var f = $p1;return Fay$$list([Fay$$_(f)(x),y]);}throw ["unhandled case in first",[$p1,$p2]];});};};Arrows.second = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p2),2)) {var x = Fay$$index(0,Fay$$_($p2));var y = Fay$$index(1,Fay$$_($p2));var f = $p1;return Fay$$list([x,Fay$$_(f)(y)]);}throw ["unhandled case in second",[$p1,$p2]];});};};Arrows.$62$$62$$62$ = function($p1){return function($p2){return new Fay$$$(function(){var g = $p2;var f = $p1;return Fay$$_(Fay$$_(Prelude.$46$)(g))(f);});};};Arrows.$42$$42$$42$ = function($p1){return function($p2){return new Fay$$$(function(){var g = $p2;var f = $p1;return Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(Fay$$_(Arrows.first)(f)))(Fay$$_(Arrows.second)(g));});};};Arrows.$38$$38$$38$ = function($p1){return function($p2){return new Fay$$$(function(){var g = $p2;var f = $p1;return function($p1){var x = $p1;return Fay$$list([Fay$$_(f)(x),Fay$$_(g)(x)]);};});};};Arrows.split = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$list([x,x]);});};Arrows.unsplit = new Fay$$$(function(){return Prelude.uncurry;});Arrows.liftA2 = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var g = $p3;var f = $p2;var op = $p1;return Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(Fay$$_(Fay$$_(Arrows.$38$$38$$38$)(f))(g)))(Fay$$_(Arrows.unsplit)(op));});};};};Arrows.liftA2_ = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Fay$$_(Prelude.$46$)(Arrows.liftA2))(Fay$$_(Fay$$_(Prelude.$46$)(Prelude.$_const))(Prelude.$_const))))(Fay$$_(Fay$$$_return)(Fay$$unit));});Arrows.$126$$62$ = function($p1){return function($p2){return new Fay$$$(function(){var arr = $p2;var val = $p1;return Fay$$_(arr)(val);});};};var StarCollection = {};StarCollection.playerSpeed = 100;StarCollection._Player = function Player(sprite,score,scoreText,emitter){this.sprite = sprite;this.score = score;this.scoreText = scoreText;this.emitter = emitter;};StarCollection._Player.prototype.instance = "Player";StarCollection.Player = function(sprite){return function(score){return function(scoreText){return function(emitter){return new Fay$$$(function(){return new StarCollection._Player(sprite,score,scoreText,emitter);});};};};};StarCollection.sprite = function(x){return new Fay$$$(function(){return Fay$$_(x).sprite;});};StarCollection.score = function(x){return new Fay$$$(function(){return Fay$$_(x).score;});};StarCollection.scoreText = function(x){return new Fay$$$(function(){return Fay$$_(x).scoreText;});};StarCollection.emitter = function(x){return new Fay$$$(function(){return Fay$$_(x).emitter;});};StarCollection._GameState = function GameState(physics,player1,player2,platforms,stars){this.physics = physics;this.player1 = player1;this.player2 = player2;this.platforms = platforms;this.stars = stars;};StarCollection._GameState.prototype.instance = "GameState";StarCollection.GameState = function(physics){return function(player1){return function(player2){return function(platforms){return function(stars){return new Fay$$$(function(){return new StarCollection._GameState(physics,player1,player2,platforms,stars);});};};};};};StarCollection.physics = function(x){return new Fay$$$(function(){return Fay$$_(x).physics;});};StarCollection.player1 = function(x){return new Fay$$$(function(){return Fay$$_(x).player1;});};StarCollection.player2 = function(x){return new Fay$$$(function(){return Fay$$_(x).player2;});};StarCollection.platforms = function(x){return new Fay$$$(function(){return Fay$$_(x).platforms;});};StarCollection.stars = function(x){return new Fay$$$(function(){return Fay$$_(x).stars;});};StarCollection.preloadGame = function($p1){return new Fay$$$(function(){var game = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Prelude.putStrLn)(Fay$$list("Preloading....."))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude.mapM_)(Fay$$_(Fay$$_(Prelude.$36$)(Prelude.uncurry))(Fay$$_(PhaserHS.loadImage)(game))))(Fay$$list([Fay$$list([Fay$$list("ground"),Fay$$list("platform.png")]),Fay$$list([Fay$$list("star"),Fay$$list("star.png")]),Fay$$list([Fay$$list("bluepart"),Fay$$list("bluepart.png")]),Fay$$list([Fay$$list("redpart"),Fay$$list("redpart.png")])]))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(PhaserHS.loadSpriteSheet)(game))(Fay$$list("dudeblue")))(Fay$$list("blueplayer.png")))(Fay$$list([20,32]))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(PhaserHS.loadSpriteSheet)(game))(Fay$$list("dudered")))(Fay$$list("redplayer.png")))(Fay$$list([20,32]))))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(PhaserHS.loadBitmapFont)(game))(Fay$$list("testfont")))(Fay$$list("font.png")))(Fay$$list("font.fnt"))))));});};StarCollection.createPlayer = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return function($p6){return new Fay$$$(function(){var effectTexName = $p6;var texName = $p5;var textPos = $p4;var startPos = $p3;var physics = $p2;var game = $p1;return (function(){var createScoreDisplay = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(PhaserHS.newText)(game))(Fay$$list("testfont")))(64))(textPos))(Fay$$list("0"));});var createEffect = function($p1){return new Fay$$$(function(){var particleTexName = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(PhaserHS.newEmitter)(game))(Fay$$list([0,0])))(50))(Fay$$list([particleTexName]))))(function($p1){var emitter = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(PhaserHS.setParticleMaxSpeed)(emitter))(Fay$$list([(-(400)),(-(400))]))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(PhaserHS.setParticleMinSpeed)(emitter))(Fay$$list([800,800]))))(Fay$$_(Fay$$$_return)(emitter)));});});};return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Fay$$_(PhaserHS.newSprite)(game))(texName))(Fay$$list([50,50]))))(function($p1){var sprite = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(PhaserHS.enable)(physics))(sprite)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Arrows.$126$$62$)(sprite))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.body))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.gravity))(Fay$$_(PhaserHS.setY)(300))))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Arrows.$126$$62$)(sprite))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.anchor))(Fay$$_(PhaserHS.setTo)(Fay$$list([0.5,0.5]))))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Arrows.$126$$62$)(sprite))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.position))(Fay$$_(PhaserHS.setTo)(startPos)))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Arrows.$126$$62$)(sprite))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.scale))(Fay$$_(PhaserHS.setTo)(Fay$$list([3,3]))))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Arrows.$126$$62$)(sprite))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.body))(Fay$$_(Fay$$_(Prelude.flip)(PhaserHS.collideWorldBounds))(true)))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(PhaserHS.addAnimation)(sprite))(Fay$$list("walk")))(Prelude.enumFromTo(0)(7)))(10))(true)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(PhaserHS.playAnimation)(sprite))(Fay$$list("walk"))))(Fay$$_(Fay$$_(Data.Function.$60$$42$$62$)(Fay$$_(Fay$$_(Data.Function.$60$$42$$62$)(Fay$$_(Fay$$_(Data.Function.$60$$42$$62$)(Fay$$_(Fay$$_(Data.Function.fmap)(StarCollection.Player))(Fay$$_(Fay$$$_return)(sprite))))(Fay$$_(Fay$$$_return)(0))))(createScoreDisplay)))(Fay$$_(createEffect)(effectTexName))))))))));});})();});};};};};};};StarCollection.createGame = function($p1){return new Fay$$$(function(){var game = $p1;return (function(){var createStar = function($p1){return function($p2){return new Fay$$$(function(){var stars = $p2;var game = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Data.Function.fmap)(function($p1){var $gen_0 = $p1;return Fay$$_(Fay$$_(Fay$$mult)($gen_0))(0.2);}))(PhaserHS.random)))(function($p1){var rBounce = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Data.Function.fmap)(function($p1){var i = $p1;return Fay$$_(Fay$$_(Fay$$add)(80))(Fay$$_(Fay$$_(Fay$$mult)(i))(Fay$$_(Fay$$_(Fay$$sub)(800))(Fay$$_(Fay$$_(Fay$$mult)(80))(2))));}))(PhaserHS.random)))(function($p1){var rx = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Fay$$_(PhaserHS.create)(stars))(Fay$$list("star")))(Fay$$list([rx,0]))))(function($p1){var star = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Arrows.$126$$62$)(star))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.anchor))(Fay$$_(PhaserHS.setTo)(Fay$$list([0.5,0.5]))))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Arrows.$126$$62$)(star))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.scale))(Fay$$_(PhaserHS.setTo)(Fay$$list([3,3]))))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Arrows.$126$$62$)(star))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.body))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.gravity))(Fay$$_(PhaserHS.setY)(80))))))(Fay$$_(Fay$$_(Arrows.$126$$62$)(star))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.body))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.bounce))(Fay$$_(PhaserHS.setY)(Fay$$_(Fay$$_(Fay$$add)(0.7))(rBounce))))))));});});});});};};return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Prelude.putStrLn)(Fay$$list("Creating..."))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(PhaserHS.startGamePad)(game)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(PhaserHS.setSmoothing)(game))(false)))(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(PhaserHS.startPhysics)(game))(PhaserHS.arcadePhysics)))(function($p1){var physics = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(PhaserHS.newGroup)(game)))(function($p1){var platforms = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(PhaserHS.enableBody)(platforms))(true)))(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Fay$$_(PhaserHS.create)(platforms))(Fay$$list("ground")))(Fay$$list([0,Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Fay$$_(Prelude.$46$)(PhaserHS.height))(PhaserHS.world)))(game)))(64)]))))(function($p1){var ground = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(PhaserHS.setWidth)(ground))(800)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(PhaserHS.setHeight)(ground))(64)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(PhaserHS.setImmovable)(Fay$$_(PhaserHS.body)(ground)))(true)))((function(){return new Fay$$$(function(){var cp = new Fay$$$(function(){return Fay$$_(Fay$$_(StarCollection.createPlayer)(game))(physics);});return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(cp)(Fay$$list([740,50])))(Fay$$list([710,50])))(Fay$$list("dudeblue")))(Fay$$list("bluepart"))))(function($p1){var player1$39$ = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(cp)(Fay$$list([60,50])))(Fay$$list([30,50])))(Fay$$list("dudered")))(Fay$$list("redpart"))))(function($p1){var player2$39$ = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(PhaserHS.newGroup)(game)))(function($p1){var stars = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(PhaserHS.enableBody)(stars))(true)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Fay$$_(Fay$$_(PhaserHS.repeatTimer)(game))(Fay$$_(Fay$$_(Fay$$mult)(2))(PhaserHS.seconds)))(5)))(Fay$$_(Fay$$_(createStar)(game))(stars))))(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(PhaserHS.newText)(game))(Fay$$list("testfont")))(64))(Fay$$list([260,100])))(Fay$$list("Collect\n stars!"))))(function($p1){var txt = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Fay$$_(PhaserHS.singleShot)(game))(Fay$$_(Fay$$_(Fay$$mult)(3))(PhaserHS.seconds))))(Fay$$_(PhaserHS.destroy)(txt))))(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$$_return))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(StarCollection.GameState)(physics))(player1$39$))(player2$39$))(platforms))(stars)));})));});});});});})())));}));});}))));})();});};StarCollection.addScore = function($p1){return function($p2){return new Fay$$$(function(){var p = $p2;var x = $p1;return (function(){var $36$_record_to_update = Object.create(Fay$$_(p));$36$_record_to_update.score = Fay$$_(Fay$$_(Fay$$add)(Fay$$_(StarCollection.score)(p)))(x);return $36$_record_to_update;})();});};};StarCollection.addScore1 = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var state = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(PhaserHS.modify)(state)))(function($p1){var g = $p1;return (function(){var $36$_record_to_update = Object.create(Fay$$_(g));$36$_record_to_update.player1 = Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(StarCollection.addScore)(x)))(Fay$$_(StarCollection.player1)(g));return $36$_record_to_update;})();})))(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(PhaserHS.get)(state)))(function($p1){var currentState = $p1;return (function(){return new Fay$$$(function(){var player = new Fay$$$(function(){return Fay$$_(StarCollection.player1)(currentState);});return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(PhaserHS.setText)(Fay$$_(StarCollection.scoreText)(player))))(Fay$$_(Prelude.show)(Fay$$_(StarCollection.score)(player)));});})();}));});};};StarCollection.addScore2 = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var state = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(PhaserHS.modify)(state)))(function($p1){var g = $p1;return (function(){var $36$_record_to_update = Object.create(Fay$$_(g));$36$_record_to_update.player2 = Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(StarCollection.addScore)(x)))(Fay$$_(StarCollection.player2)(g));return $36$_record_to_update;})();})))(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(PhaserHS.get)(state)))(function($p1){var currentState = $p1;return (function(){return new Fay$$$(function(){var player = new Fay$$$(function(){return Fay$$_(StarCollection.player2)(currentState);});return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(PhaserHS.setText)(Fay$$_(StarCollection.scoreText)(player))))(Fay$$_(Prelude.show)(Fay$$_(StarCollection.score)(player)));});})();}));});};};StarCollection.updateGame = function($p1){return function($p2){return new Fay$$$(function(){var state = $p2;var game = $p1;return (function(){var enumerate = new Fay$$$(function(){return Fay$$_(Prelude.zip)(Prelude.enumFrom(1));});var starCollisionHandler = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var star = $p4;var player = $p2;var addScore = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(addScore)(state))(1)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(createParticleBurst)(Fay$$_(StarCollection.emitter)(player)))(Fay$$_(Fay$$_(Prelude.$36$)(PhaserHS.vectorToTuple))(Fay$$_(PhaserHS.position)(star)))))(Fay$$_(PhaserHS.kill)(star)));});};};};};var createParticleBurst = function($p1){return function($p2){return new Fay$$$(function(){var pos = $p2;var emitter = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(PhaserHS.setEmitterPos)(emitter))(pos)))(Fay$$_(Fay$$_(Fay$$_(PhaserHS.emitterBurst)(emitter))(2000))(25));});};};var updatePlayer = function($p1){return function($p2){return new Fay$$$(function(){var player$39$ = $p2;var pad = $p1;if (Fay$$_(Fay$$_(Fay$$_(Arrows.$126$$62$)(pad))(PhaserHS.padRight))) {return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Arrows.$126$$62$)(player$39$))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.body))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.velocity))(Fay$$_(PhaserHS.setX)(StarCollection.playerSpeed))))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Arrows.$126$$62$)(player$39$))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.scale))(Fay$$_(PhaserHS.setX)(3)))))(Fay$$_(Fay$$_(PhaserHS.playAnimation)(player$39$))(Fay$$list("walk"))));} else {if (Fay$$_(Fay$$_(Fay$$_(Arrows.$126$$62$)(pad))(PhaserHS.padLeft))) {return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Arrows.$126$$62$)(player$39$))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.body))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.velocity))(Fay$$_(PhaserHS.setX)((-(Fay$$_(StarCollection.playerSpeed)))))))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(PhaserHS.playAnimation)(player$39$))(Fay$$list("walk"))))(Fay$$_(Fay$$_(Arrows.$126$$62$)(player$39$))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.scale))(Fay$$_(PhaserHS.setX)((-(3)))))));} else {if (Fay$$_(Prelude.otherwise)) {return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(PhaserHS.setFrame)(player$39$))(2)))(Fay$$_(Fay$$_(Arrows.$126$$62$)(player$39$))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.body))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.velocity))(Fay$$_(PhaserHS.setX)(0)))));}}}});};};return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(PhaserHS.get)(state)))(function($p1){if (Fay$$_($p1) instanceof StarCollection._GameState) {var physics = Fay$$_($p1).physics;var platforms = Fay$$_($p1).platforms;var player1 = Fay$$_($p1).player1;var player2 = Fay$$_($p1).player2;var stars = Fay$$_($p1).stars;return (function(){return new Fay$$$(function(){var collider = new Fay$$$(function(){return Fay$$_(PhaserHS.collide)(physics);});return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(collider)(Fay$$_(StarCollection.sprite)(player1)))(platforms)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(collider)(Fay$$_(StarCollection.sprite)(player2)))(platforms)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(collider)(stars))(platforms)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Fay$$_(Fay$$_(PhaserHS.overlap)(physics))(Fay$$_(StarCollection.sprite)(player1)))(stars)))(Fay$$_(Fay$$_(starCollisionHandler)(StarCollection.addScore1))(player1))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Fay$$_(Fay$$_(PhaserHS.overlap)(physics))(Fay$$_(StarCollection.sprite)(player2)))(stars)))(Fay$$_(Fay$$_(starCollisionHandler)(StarCollection.addScore2))(player2))))(Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Prelude.forM_)(Fay$$_(enumerate)(Fay$$list([player1,player2])))))(function($p1){if (Fay$$listLen(Fay$$_($p1),2)) {var i = Fay$$index(0,Fay$$_($p1));var player = Fay$$index(1,Fay$$_($p1));return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(PhaserHS.getGamePadInput)(game))(i)))(function($p1){var input = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(updatePlayer)(input))(Fay$$_(StarCollection.sprite)(player))))((function(){return new Fay$$$(function(){var onGround = new Fay$$$(function(){return Fay$$_(Fay$$_(Arrows.$126$$62$)(player))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(StarCollection.sprite))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.body))(PhaserHS.touchingDown)));});return Fay$$_(Fay$$_(Prelude.$36$)(Fay$$_(Prelude.when)(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(PhaserHS.padUp)(input)))(onGround))))(Fay$$_(Fay$$_(Arrows.$126$$62$)(player))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(StarCollection.sprite))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.body))(Fay$$_(Fay$$_(Arrows.$62$$62$$62$)(PhaserHS.velocity))(Fay$$_(PhaserHS.setY)((-(350))))))));});})());});}throw ["unhandled case",$p1];}))))));});})();}throw ["unhandled case",$p1];});})();});};};StarCollection.startGame = function($p1){return new Fay$$$(function(){var event = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(PhaserHS.newGame)(800))(600)))(function($p1){var game = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(PhaserHS.newState)(game))(Fay$$list("Mainmenu")))(StarCollection.preloadGame))(StarCollection.createGame))(StarCollection.updateGame)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(PhaserHS.changeState)(game))(Fay$$list("Mainmenu"))))(Fay$$_(Fay$$$_return)(Fay$$unit)));});});};StarCollection.main = new Fay$$$(function(){return Fay$$_(Fay$$_(DOM.addWindowEvent)(Fay$$list("load")))(StarCollection.startGame);});Fay$$objConcat(Fay$$fayToJsHash,{"Player": function(type,argTypes,_obj){var obj_ = {"instance": "Player"};var obj_sprite = Fay$$fayToJs(["user","Sprite",[]],_obj.sprite);if (undefined !== obj_sprite) {obj_['sprite'] = obj_sprite;}var obj_score = Fay$$fayToJs_int(_obj.score);if (undefined !== obj_score) {obj_['score'] = obj_score;}var obj_scoreText = Fay$$fayToJs(["user","BitmapText",[]],_obj.scoreText);if (undefined !== obj_scoreText) {obj_['scoreText'] = obj_scoreText;}var obj_emitter = Fay$$fayToJs(["user","Emitter",[]],_obj.emitter);if (undefined !== obj_emitter) {obj_['emitter'] = obj_emitter;}return obj_;},"GameState": function(type,argTypes,_obj){var obj_ = {"instance": "GameState"};var obj_physics = Fay$$fayToJs(["user","Physics",[]],_obj.physics);if (undefined !== obj_physics) {obj_['physics'] = obj_physics;}var obj_player1 = Fay$$fayToJs(["user","Player",[]],_obj.player1);if (undefined !== obj_player1) {obj_['player1'] = obj_player1;}var obj_player2 = Fay$$fayToJs(["user","Player",[]],_obj.player2);if (undefined !== obj_player2) {obj_['player2'] = obj_player2;}var obj_platforms = Fay$$fayToJs(["user","Group",[]],_obj.platforms);if (undefined !== obj_platforms) {obj_['platforms'] = obj_platforms;}var obj_stars = Fay$$fayToJs(["user","Group",[]],_obj.stars);if (undefined !== obj_stars) {obj_['stars'] = obj_stars;}return obj_;}});Fay$$objConcat(Fay$$jsToFayHash,{"Player": function(type,argTypes,obj){return new StarCollection._Player(Fay$$jsToFay(["user","Sprite",[]],obj["sprite"]),Fay$$jsToFay_int(obj["score"]),Fay$$jsToFay(["user","BitmapText",[]],obj["scoreText"]),Fay$$jsToFay(["user","Emitter",[]],obj["emitter"]));},"GameState": function(type,argTypes,obj){return new StarCollection._GameState(Fay$$jsToFay(["user","Physics",[]],obj["physics"]),Fay$$jsToFay(["user","Player",[]],obj["player1"]),Fay$$jsToFay(["user","Player",[]],obj["player2"]),Fay$$jsToFay(["user","Group",[]],obj["platforms"]),Fay$$jsToFay(["user","Group",[]],obj["stars"]));}});Fay$$_(StarCollection.main, true);
