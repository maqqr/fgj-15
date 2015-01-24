var jammu;
var jimmu;
var lvlWidth = 800;
var lvlHeight = 600;
var game;
var cursors;
var speed = 200;
//var maxScore = 5;
var maxTime = 30;
var startTime = 1;
var startingText = "Shoot your opponent!";
var startingTextComponent;
var player1Text;
var player2Text;
var amountOfSpikes = 5;
var spikes;
var player2StartX = 75;
var player2StartY = 100;
var player1StartX = 725;
var player1StartY = 500;
var balls = [];
var gameEnded = false;
init();

function init(){
	game = new Phaser.Game(lvlWidth, lvlHeight, Phaser.AUTO, '', { preload: preload, create: create, update: update });
}

function preload()
{
	game.load.image('jammu', './../../assets/Player1.png');
	game.load.image('jimmu', './../../assets/Player2.png');
	game.load.image('spikes', './../../assets/Spikes.png');
	game.load.image('ball', './../../assets/Ball.png');
	game.load.image('ballBlue', './../../assets/BallBlue.png');
}


function create(){
	game.physics.startSystem(Phaser.Physics.ARCADE);
	game.stage.backgroundColor = '#339933';
	jammu = createPlayer('jammu', 1, false, player1StartX, player1StartY);
	jimmu = createPlayer('jimmu', 2, false, player2StartX, player2StartY);	
	jimmu.sprite.body.bounce.setTo(1,1);
	jammu.sprite.body.bounce.setTo(1,1);
	//game.physics.arcade.enable(jimmu);
	//spikes.enableBody = true;
	//spikes.physicsBodyType = Phaser.Physics.ARCADE;
	spikes = [];
	
	game.time.events.add(Phaser.Timer.SECOND * startTime, start, this);
	createSpikes();
	
	player2Text = game.add.text(lvlWidth *0.1 -50, 50, "Game on!", { fontSize: '22px', fill: '#fff' });
	player1Text = game.add.text(lvlWidth *0.75 -50, 50, "Game on!", { fontSize: '22px', fill: '#fff' });
	startingTextComponent = game.add.text(lvlWidth *0.2-50, lvlHeight * 0.5 -10, startingText, { fontSize: '22px', fill: '#fff' });
}

function createSpikes(){
	for(var i = 0; i < amountOfSpikes; i++){
		var spike = game.add.sprite(game.rnd.integerInRange(player1StartX-50, player2StartX+50), game.rnd.integerInRange(0, 600), 'spikes');
		game.physics.arcade.enable(spike);
		spike.body.immovable = true;
		spikes[spikes.length] = spike;
	}
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



function update(){
	if(game.time.totalElapsedSeconds() >= maxTime + startTime) endGame();
	game.physics.arcade.collide(jimmu.sprite, jammu.sprite);
	for	(var i = 0; i < balls.length; i++){
		for(var j = 0; j < spikes.length; j++){
			game.physics.arcade.overlap(spikes[j], balls[i],function(){
				spikes[j].destroy();
				balls[i].destroy();
			}, null, null)
		}
		game.physics.arcade.collide(jimmu.sprite, balls[i], function()
		{
			jimmu.sprite.destroy();
			jammu.score++;
			game.time.events.add(Phaser.Timer.SECOND * 1, function(){
			endGame();
			}, this);
		}, null, null);
		game.physics.arcade.collide(jammu.sprite, balls[i], function()
		{
			jammu.sprite.destroy();
			jimmu.score++;
			game.time.events.add(Phaser.Timer.SECOND * 1, function(){
			endGame();
			}, this);
		}, null, null);
		
	}


	
	player1Text.text = defineText(1);
	player2Text.text = defineText(2);
	jimmu.update(game, balls);
	jammu.update(game, balls);
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

function endGame(){
	if(gameEnded) return;
	gameEnded = true;
	game.add.text(lvlWidth *0.5 -50, lvlHeight * 0.5 -10, 'Game Over!', { fontSize: '22px', fill: '#fff' });
	
	balls.forEach(function(element, index, array){
		element.body.velocity.x = 0;
		element.body.velocity.y = 0;
	});
	
	jimmu.setActivity(false);
	jammu.setActivity(false);
	
	if(jammu.score > jimmu.score)
		result = 1;
	else if(jammu.score < jimmu.score)
		result = 2;
	else
		result = 0;
		
	parent.$(parent.document).trigger("onGameEnd",result);
}


function destroy(gameResult){
	parent.$(parent.document).trigger("onGameEnd",gameResult);

}

