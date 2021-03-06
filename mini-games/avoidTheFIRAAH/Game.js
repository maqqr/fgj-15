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
var gameEnded = false;
var layer;
var balls;
var map;

function preload()
{
	game.load.tilemap('level', 'level.json', null, Phaser.Tilemap.TILED_JSON);
	game.load.image('jammu', './../../assets/Player1.png');
	game.load.image('jimmu', './../../assets/Player2.png');
	game.load.image('ball', './../../assets/BallBlue.png');
	game.load.image('violetTiles', 'violetTiles.png');
	//game.load.image('lava', './../../assets/Lava.png');
	game.load.script('filter', 'Fire.js');
}


function create(){
	game.physics.startSystem(Phaser.Physics.ARCADE);
	balls = new Array();
	createBackground();
	createLevel();
	
	jammu = createPlayer('jammu', 1, false, 650, 500);
	jimmu = createPlayer('jimmu', 2, false, 150, 100);	
	jimmu.setBounce(100,100);
	jammu.setBounce(100,100);
	//game.physics.arcade.enable(jimmu);
	//spikes.enableBody = true;
	//spikes.physicsBodyType = Phaser.Physics.ARCADE;

	game.time.events.add(Phaser.Timer.SECOND * startTime, start, this);
	game.time.events.repeat(Phaser.Timer.SECOND * 2, 10, createRandomBall, this);
	
	player1Text = game.add.text(lvlWidth *0.85 -50, 50, "P1 has collected "+jammu.score+" balls!", { fontSize: '22px', fill: '#fff' });
	player2Text = game.add.text(lvlWidth *0.1 -50, 50, "P2 has collected "+jimmu.score+" balls!", { fontSize: '22px', fill: '#fff' });
	startingTextComponent = game.add.text(lvlWidth *0.2-50, lvlHeight * 0.5 -10, startingText, { fontSize: '22px', fill: '#fff' });
}

function createRandomBall(){
	var x = game.rnd.integerInRange(0, 1) === 1 ? 25 : 775;
	var ball = game.add.sprite(x, game.rnd.integerInRange(0+25, 600-25), 'ball');
	game.physics.arcade.enable(ball);
	ball.body.velocity.x = x === 25 ? 25 * game.rnd.integerInRange(5, 30) :
	 -25 * game.rnd.integerInRange(5, 30);
	balls[balls.length] = ball;
	


}


function createLevel(){
	map = game.add.tilemap('level');
    map.addTilesetImage('violetTiles');

    map.setCollisionByExclusion([ 0 ]);

    layer = map.createLayer('Tile Layer 1');

    //  Un-comment this on to see the collision tiles
    // layer.debug = true;

    layer.resizeWorld();

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


function start(){
	jimmu.setActivity(true);
	jammu.setActivity(true);
	startingTextComponent.destroy();
}


function createPlayer(spriteName, playerKeyboardNumber, activity, x, y){
	var player = new Player(spriteName , game, speed, x, y, playerKeyboardNumber);
	player.setActivity(activity);
	player.registerPlayerAs(playerKeyboardNumber);
	return player;
}


function defineText(playerNumber){
	var text = "Game on!";
	if(playerNumber == 1){
		if(jammu.score >= 1){
		text = "You have won!";
		
		}
		else if( jimmu.score >= 1){
		 text = "You have lost";
		}
	}
	else{
		if(jimmu.score >= 1){
		text = "You have won!";
		
		}
		else if( jammu.score >= 1){
		 text = "You have lost";
		}
	}
	return text;
}


function getTileAmount(body){
	var tiles = layer.getRayCastTiles(new Phaser.Line(body.x +20 , body.y+20, body.x + body.width-20, body.y + body.height-20));
	return tiles.length;
}


function pushAway(ballBody, pushedBody){
	var xSpeed = ballBody.x - pushedBody.x;
	pushedBody.x +=  -xSpeed;
}


function update(){
	if(game.time.totalElapsedSeconds() >= maxTime + startTime) endGame();
	game.physics.arcade.collide(jimmu.sprite, jammu.sprite);
	for(var i = 0; i < balls.length; i++){
		if(game.physics.arcade.collide(balls[i], jammu.sprite))	{
			pushAway(balls[i].body, jammu.sprite.body);
		}
		if(game.physics.arcade.collide(balls[i], jimmu.sprite)){
			pushAway(balls[i].body, jimmu.sprite.body);
		}
	}
	
	if( getTileAmount(jimmu.sprite.body) === 0){
		jimmu.setActivity(false);
		jammu.score++;
		game.time.events.add(Phaser.Timer.SECOND * 1, function(){
			endGame();
			}, this);
	}
	
	if( getTileAmount(jammu.sprite.body) === 0){
		jammu.setActivity(false);
		jimmu.score++;
		game.time.events.add(Phaser.Timer.SECOND * 1, function(){
			endGame();
			}, this);
	}
	
	
	filter.update();
	
	player1Text.text = defineText(1);
	player2Text.text = defineText(2);
	jimmu.update(game);
	jammu.update(game);
}


function endGame(){
	if(gameEnded) return;
	gameEnded = true;
	game.add.text(lvlWidth *0.5 -50, lvlHeight * 0.5 -10, 'Game Over!', { fontSize: '22px', fill: '#fff' });
	
	jimmu.setActivity(false);
	jammu.setActivity(false);
	
	var result = 0;
	
	if(jammu.score > jimmu.score)
		result = 1;
	else if(jammu.score < jimmu.score)
		result = 2;
	else
		result = 0;
		
		
	destroy(result);
}


function destroy(gameResult){
	parent.$(parent.document).trigger("onGameEnd",gameResult);
	game.destroy();

}
