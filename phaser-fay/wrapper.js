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

function getButton(pad, index) {
    if (!(pad === null || pad === undefined)) {
        var rpad = pad._rawPad;
        if (!(rpad === null || rpad === undefined)) {
            return rpad.buttons[index] == 1;
        }
    }
    return false;
}

// :: Game -> (Double, Double) -> Int -> [String] -> Fay Emitter
function wrNewEmitter(game, pos, maxPart, texNames) {
    var em = game.add.emitter(pos[0], pos[1], maxPart);
    em.makeParticles(texNames);
    return em;
}

// :: Emitter -> (Double, Double) -> Fay ()
function wrSetEmitterPos(emitter, pos) {
    emitter.x = pos[0];
    emitter.y = pos[1];
}

// :: Game -> Int -> Fay GamePadInput
function wrGetGamePadInput(game, padIndex) {
    var pad = [game.input.gamepad.pad1, game.input.gamepad.pad2][padIndex];
    var ka = Phaser.Keyboard.O;
    var kb = Phaser.Keyboard.L;
    var kleft = Phaser.Keyboard.LEFT;
    var kright = Phaser.Keyboard.RIGHT;
    var kdown = Phaser.Keyboard.DOWN;
    var kup = Phaser.Keyboard.UP;
    if (padIndex == 2) {
        ka = Phaser.Keyboard.Q;
        kb = Phaser.Keyboard.A;
        kleft = Phaser.Keyboard.A;
        kright = Phaser.Keyboard.D;
        kdown = Phaser.Keyboard.S;
        kup = Phaser.Keyboard.W;
    }

    return { a:     getButton(pad, 1)  || game.input.keyboard.isDown(ka)
           , b:     getButton(pad, 0)  || game.input.keyboard.isDown(kb)
           , left:  getButton(pad, 14) || game.input.keyboard.isDown(kleft)
           , right: getButton(pad, 15) || game.input.keyboard.isDown(kright)
           , down:  getButton(pad, 16) || game.input.keyboard.isDown(kdown)
           , up:    getButton(pad, 17) || game.input.keyboard.isDown(kup)
           };
}

// :: Emitter -> (Double, Double) -> Fay ()
function wrSetParticleMaxSpeed(emitter, speed) {
    emitter.maxParticleSpeed = new Phaser.Point(speed[0], speed[1]);
}

// :: Emitter -> (Double, Double) -> Fay ()
function wrSetParticleMinSpeed(emitter, speed) {
    emitter.minParticleSpeed = new Phaser.Point(speed[0], speed[1]);
}