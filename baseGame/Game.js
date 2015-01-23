var jammu;
var jimmu;
var lvlWidth = 800;
var lvlHeight = 600;
var game = new Phaser.Game(lvlWidth, lvlHeight, Phaser.AUTO, '', { preload: preload, create: create, update: update });
var cursors;
var speed = 10;
var maxScore = 5;
var maxTime = 10;
var startTime = 1;

function preload()
{
	game.load.image('jammu', 'Jammu.png');
	game.load.image('jimmu', 'Jimmu.png');
	cursors = game.input.keyboard.createCursorKeys();
}


function create(){
	jammu = createPlayer('jammu', 1, false);
	jimmu = createPlayer('jimmu', 2, false);	//game.physics.arcade.enable(jimmu);
    game.time.events.add(Phaser.Timer.SECOND * startTime, start, this);
}

function start(){
	jimmu.setActivity(true);
	jammu.setActivity(true);
}


function createPlayer(spriteName, playerNumber, activity){
	var player = new Player(spriteName , game, speed);
	player.setActivity(activity);
	player.registerPlayerAs(playerNumber);
	return player;
}



function update(){
	if(game.time.totalElapsedSeconds() >= maxTime + startTime) destroy();

	jimmu.update(game);
	jammu.update(game);
}


function destroy(){
	game.add.text(lvlWidth *0.5 -50, lvlHeight * 0.5 -10, 'Game Over!', { fontSize: '22px', fill: '#fff' });
	
	jimmu.setActivity(false);
	jammu.setActivity(false);
}
