/*
 * A simple wrapper for phaser.js that simplifies
 * Fay foreign function interface.
 */


// :: Int -> Int -> Fay Game
function wrNewGame(width, height) {
    return new Phaser.Game(width, height, Phaser.AUTO, '');
};

// :: Game -> String -> (Game -> Fay ()) -> (Game -> Fay ()) -> (Game -> Fay ()) -> Fay State
function wrNewState(game, name, preloadFunc, createFunc, updateFunc) {
    var state = function(game) { };
    state.stateName = name;
    state.prototype = {
        preload: function() { preloadFunc(game); },
        create: function() { var s = createFunc(game); this.internalState = s; },
        update: function() { updateFunc(game, game.state.getCurrentState()); }
    };
    game.state.add(name, state);
    return state;
};

// :: Game -> String -> (Double, Double) -> Fay Sprite
function wrNewSprite(game, texName, pos) {
    return game.add.sprite(pos[0], pos[1], texName);
};

// :: Game -> String -> (Int, Int) -> Fay ()
function wrLoadSpriteSheet(game, texName, texPath, size) {
    game.load.spritesheet(texName, texPath, size[0], size[1]);
};

// :: Group -> (Double, Double) -> Fay Sprite
function wrCreate(group, texName, pos) {
    return group.create(pos[0], pos[1], texName);
}

// func startPhys, return game.physics.arcade!
// :: PhysicsType -> Physics
function wrStartPhysics(game, physType) {
    game.physics.startSystem(physType);
    // TODO: return correct physics depending on physType
    return game.physics.arcade;
}

function evalThunk(x) {
    if (typeof x.value === "function") {
        var xx = x.value();
        return evalThunk(xx);
    }
    return x;
}

// :: Physics -> Ptr a -> Ptr b -> Fay Bool
function wrCollide(physics, obj1, obj2) {
    return physics.collide(evalThunk(obj1), evalThunk(obj2));
}

// :: Physics -> Ptr a -> Ptr b -> (a -> b -> Fay ()) -> Fay Bool
function wrOverlap(physics, obj1, obj2, callback) {
    return physics.overlap(evalThunk(obj1), evalThunk(obj2), callback, null);
}

// :: (Double, Double) -> Vector -> Fay ()
function wrSetTo(pos, vec) {
    vec.setTo(pos[0], pos[1]);
}

// :: Game -> String -> Double -> (Double, Double) -> String -> Fay BitmapText
function wrNewText(game, name, fntSize, pos, text) {
    return game.add.bitmapText(pos[0], pos[1], name, text, fntSize);
}

function getButton(rawpad, index) {
    if (rawpad === null || rawpad === undefined) {
        return false;
    }
    else {
        return rawpad.buttons[index] == 1;
    }
}

// :: GamePad -> Fay GamePadInput
function wrGetGamePadInput(pad) {
    return { a: getButton(pad._rawPad, 1)
           , b: getButton(pad._rawPad, 0)
           };
}
