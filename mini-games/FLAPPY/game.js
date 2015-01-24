var game = new Phaser.Game(800, 600, Phaser.AUTO, '', { preload: preload, create: create, update: update });
var bird1;
var bird2;
var topp;
var bottom;
var startTime = 1;
var maxTime = 10;
var bottomStartY = 470;
var bottomEndY = 320;
var topStartY = 200;
var topEndY = 320;

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
	//this.insctructionText = game.add.text(250, 200, 'PRESS MORE BUTTONS!', { fontSize: '50', fill: '#000'});
    game.time.events.add(Phaser.Timer.SECOND * startTime, start, this);
    game.physics.arcade.gravity.y = 750;
    createBird(375,200,1);
}

function update() {
	if(game.time.totalElapsedSeconds() > startTime) {
		bird1.addPhysics();
	}
	if(game.time.totalElapsedSeconds() >= maxTime + startTime) end();
	if(game.time.totalElapsedSeconds() < (maxTime + startTime) && game.time.totalElapsedSeconds() > startTime)
		moveFloors();
	//bird1.update(game);
	//bird2.update(game);

}

function createBird(x, y, playerNumber) {
	bird1 = new Bird(game, x, y);
	if (playerNumber != 0)
		bird1.registerBirdAs(playerNumber);
	return bird;
}

function start() {
}

function end() {
	//game.add.text(800 *0.5 -85, 600 * 0.5 - 55, 'GAME OVER!', { fontSize: '22px', fill: '#f00' });
}

function moveFloors() {
	var progression = (game.time.totalElapsedSeconds() - startTime) / (maxTime);

	bottom.y = bottomStartY - (150 * progression);
	topp.y = topStartY + (150 * progression);
}