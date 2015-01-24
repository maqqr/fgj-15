var game = new Phaser.Game(800, 600, Phaser.AUTO, '', { preload: preload, create: create, update: update });
var controller1;
var controller2;
var nextButtonController1;
var nextButtonController2;
var startTime = 1;
var maxTime = 10;

function preload() {
    game.load.image('right', 'right.png');
    game.load.image('up', 'up.png');
    game.load.image('down', 'down.png');
    game.load.image('left', 'left.png');
    game.load.image('a', 'a.png');
    game.load.image('b', 'b.png');
    game.load.image('controller', 'controller.png');
    game.load.image('particle', 'controllerparticle.png');
}

function create() {
	game.stage.backgroundColor = 0xaaaaaa;
	controller1 = createController(450, 400, 1);
	controller2 = createController(62, 400, 2);
	nextButtonController1 = createController(450, 180, 0);
	nextButtonController2 = createController(62, 180, 0);
	controller1.setNextButtonController(nextButtonController1);
	controller2.setNextButtonController(nextButtonController2);
	nextButtonController1.changeNextButton();
	nextButtonController2.changeNextButton();
	this.insctructionText = game.add.text(220, 100, 'PRESS THE RIGHT BUTTONS!', { fontSize: '50', fill: '#000'});
    game.time.events.add(Phaser.Timer.SECOND * startTime, start, this);
}

function update() {
	if(game.time.totalElapsedSeconds() >= maxTime + startTime) end();
	controller1.update(game);
	controller2.update(game);
}

function createController(x, y, playerNumber) {
	var controller = new Controller(game, x, y);
	if (playerNumber != 0)
		controller.registerControllerAs(playerNumber);
	return controller;
}

function start() {
	controller1.active = true;
	controller2.active = true;
}

function end() {
	controller2.active = false;
	controller1.active = false;
	game.add.text(800 *0.5 -85, 600 * 0.5 + 50, 'GAME OVER!', { fontSize: '22px', fill: '#f00' });
	if (controller1.score > controller2.score) {
		parent.$(parent.document).trigger("onGameEnd",1);
	}
	else if (controller1.score < controller2.score) {
		parent.$(parent.document).trigger("onGameEnd",2);
	}
	else
		parent.$(parent.document).trigger("onGameEnd",0);
	game.destroy();
}