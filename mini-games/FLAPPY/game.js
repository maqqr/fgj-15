var game = new Phaser.Game(800, 600, Phaser.AUTO, '', { preload: preload, create: create, update: update });
var bird1;
var bird2;
var topp;
var bottom;
var startTime = 1;
var maxTime = 20;
var bottomStartY = 470;
var bottomEndY = 320;
var topStartY = 200;
var topEndY = 320;
var birdStartPower = -400;
var floors;


function preload() {
	game.load.spritesheet('sheet', 'bird.png', 32, 32, 2);
	game.load.image('bottomground', 'bottomground.png');
	game.load.image('topground', 'topground.png');
	game.load.image('wall', 'wall.png');
}

function create() {
	game.stage.backgroundColor = 0xaaaaaa;
	topp = game.add.sprite(200, topStartY, 'topground');
	bottom = game.add.sprite(200, bottomStartY, 'bottomground');
	game.add.sprite(170, 200, 'wall');
	game.add.sprite(600, 200, 'wall');
	game.physics.startSystem(Phaser.Physics.ARCADE);
	this.insctructionText = game.add.text(235, 100, 'DO NOT HIT THE FLOORS!', { fontSize: '50', fill: '#000'});
    game.time.events.add(Phaser.Timer.SECOND * startTime, start, this);
    game.physics.arcade.gravity.y = 1000;
    bird1 = createBird(460,300,1);
    bird2 = createBird(300,300,2);
    this.active = false;
    this.gameActive = false;
}

function update() {
	if(game.time.totalElapsedSeconds() > startTime) {
		addBodies();
	}
	//if(game.time.totalElapsedSeconds() >= maxTime + startTime) end();
	if(game.time.totalElapsedSeconds() < (maxTime + startTime) && game.time.totalElapsedSeconds() > startTime) {
		moveFloors();
		diminishBirdPowers();
	}
	//bird1.update(game);
	//bird2.update(game);
    	game.physics.arcade.overlap(bird1.sheet, topp, bird1Collision, null, this);
    	game.physics.arcade.overlap(bird1.sheet, bottom, bird1Collision, null, this);
    	game.physics.arcade.overlap(bird2.sheet, topp, bird2Collision, null, this);
    	game.physics.arcade.overlap(bird2.sheet, bottom, bird2Collision, null, this);

}

function createBird(x, y, playerNumber) {

	var bird = new Bird(game, x, y);
	if (playerNumber != 0)
		bird.registerBirdAs(playerNumber, birdStartPower);
	return bird;
}

function start() {
}

function end() {
	game.add.text(800 *0.5 -85, 600 * 0.5 - 55, 'GAME OVER!', { fontSize: '22px', fill: '#f00' });
	/*if (controller1.score > controller2.score) {
		parent.$(parent.document).trigger("onGameEnd",1);
	}
	else if (controller1.score < controller2.score) {
		parent.$(parent.document).trigger("onGameEnd",2);
	}
	else
		parent.$(parent.document).trigger("onGameEnd",0);*/
	//game.destroy();
}

function moveFloors() {
	var progression = (game.time.totalElapsedSeconds() - startTime) / (maxTime);

	bottom.y = bottomStartY - (150 * progression);
	topp.y = topStartY + (150 * progression);
}

function diminishBirdPowers() {
	var progression = (game.time.totalElapsedSeconds() - startTime) / (maxTime);
	bird1.power = birdStartPower - (progression * birdStartPower);

	bird2.power = birdStartPower - (progression * birdStartPower);
}

function addBodies() {
	if (!this.active) {
		this.active = true;
		bird1.addPhysics();
		bird2.addPhysics();
    	game.physics.arcade.enableBody(topp);
    	topp.body.allowGravity = false;
    	topp.body.immovable = true;
    	game.physics.arcade.enableBody(bottom);
    	bottom.body.allowGravity = false;
    	bottom.body.immovable = true;
    	this.gameActive = true;
	}
}

function bird1Collision(bird, wall)
{
	parent.$(parent.document).trigger("onGameEnd",2);
	game.destroy();
}

function bird2Collision(bird, wall)
{
	parent.$(parent.document).trigger("onGameEnd",1);
	game.destroy();
}