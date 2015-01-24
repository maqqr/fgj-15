var jammu;
var jimmu;
var lvlWidth = 800;
var lvlHeight = 600;
var game = new Phaser.Game(lvlWidth, lvlHeight, Phaser.WEBGL, '', { preload: preload, create: create, update: update });
var cursors;
var speed = 500;
//var maxScore = 5;
var maxTime = 30;
var startTime = 1;
var startingText = "Stay on top of the platform!";
var startingTextComponent;
var player1Text;
var player2Text;
var map;

function preload()
{
	game.load.tilemap('level', 'level.json', null, Phaser.Tilemap.TILED_JSON);
	game.load.image('jammu', './../../assets/Player1.png');
	game.load.image('jimmu', './../../assets/Player2.png');
	game.load.image('violetTiles', 'violetTiles.png');
	//game.load.image('lava', './../../assets/Lava.png');
	game.load.script('filter', 'Fire.js');
}


function create(){
	game.physics.startSystem(Phaser.Physics.ARCADE);
	
	createBackground();
	createLevel();
	
	jammu = createPlayer('jammu', 1, false, 200, 100);
	jimmu = createPlayer('jimmu', 2, false, 600, 100);	
	jimmu.sprite.body.bounce.setTo(1,1);
	jammu.sprite.body.bounce.setTo(1,1);
	//game.physics.arcade.enable(jimmu);
	//spikes.enableBody = true;
	//spikes.physicsBodyType = Phaser.Physics.ARCADE;

	game.time.events.add(Phaser.Timer.SECOND * startTime, start, this);
	//createRandomBall();
	
	player1Text = game.add.text(lvlWidth *0.85 -50, 50, "P1 has collected "+jammu.score+" balls!", { fontSize: '22px', fill: '#fff' });
	player2Text = game.add.text(lvlWidth *0.1 -50, 50, "P2 has collected "+jimmu.score+" balls!", { fontSize: '22px', fill: '#fff' });
	startingTextComponent = game.add.text(lvlWidth *0.2-50, lvlHeight * 0.5 -10, startingText, { fontSize: '22px', fill: '#fff' });
}

function createLevel(){
	map = game.add.tilemap('level');

    map.addTilesetImage('violetTiles');

    map.setCollisionByExclusion([ 13, 14, 15, 16, 46, 47, 48, 49, 50, 51 ]);

    layer = map.createLayer('Tile Layer 1');

    //  Un-comment this on to see the collision tiles
    // layer.debug = true;

  //  layer.resizeWorld();

   // game.physics.arcade.gravity.y = 250;


}


function createBackground(){
	background = game.add.sprite(0, 0);
	background.width = 800;
	background.height = 600;
	
	filter = game.add.filter('Fire', 1000, 800);
	filter.alpha = 0.0;
	background.filters = [filter];
	
}


function createRandomBall(){
	ball = game.add.sprite(game.rnd.integerInRange(0+25, 800-25), game.rnd.integerInRange(0+25, 600-25), 'ball');
	game.physics.arcade.enable(ball);
	ball.body.immovable = true;

}


function start(){
	jimmu.setActivity(true);
	jammu.setActivity(true);
	startingTextComponent.destroy();
}


function createPlayer(spriteName, playerKeyboardNumber, activity, x, y){
	var player = new Player(spriteName , game, speed, x, y);
	player.setActivity(activity);
	player.registerPlayerAs(playerKeyboardNumber);
	return player;
}


function defineText(playerNumber){
	var text = "Game on!";
	if(playerNumber == 1){
		if(jammu.score === 1){
		text = "You have won!";
		
		}
		else if( jimmu.score === 1){
		 text = "You have lost";
		}
	}
	else{
		if(jimmu.score === 1){
		text = "You have won!";
		
		}
		else if( jammu.score === 1){
		 text = "You have lost";
		}
	}
	return text;
}



function update(){
	if(game.time.totalElapsedSeconds() >= maxTime + startTime) endGame();
	game.physics.arcade.collide(jimmu.sprite, jammu.sprite);

	filter.update();
	
	player1Text.text = defineText(1);
	player2Text.text = defineText(2);
	jimmu.update(game);
	jammu.update(game);
}


function endGame(){
	game.add.text(lvlWidth *0.5 -50, lvlHeight * 0.5 -10, 'Game Over!', { fontSize: '22px', fill: '#fff' });
	
	jimmu.setActivity(false);
	jammu.setActivity(false);
	
	if(jammu.score > jimmu.score)
		//Jammu won
		;
	else if(jammu.score < jimmu.score)
		//jimmu won
		;
	else
		//its a tie
		;
		
	game.time.events.add(Phaser.Timer.SECOND * 1, function(){
			destroy();
			}, this);
}


function destroy(){


}
