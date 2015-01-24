var jammu;
var jimmu;
var lvlWidth = 800;
var lvlHeight = 600;
var game;
var cursors;
var speed = 300;
//var maxScore = 5;
var maxTime = 10;
var startTime = 1;
var startingText = "Shoot your opponent!";
var startingTextComponent;
var player1Text;
var player2Text;
var amountOfSpikes = 7;
var spikes;
var player2StartX = 75;
var player2StartY = 100;
var player1StartX = 725;
var player1StartY = 500;
var gameEnded = false;
var huntedPlayer;
init();

function init(){
	game = new Phaser.Game(lvlWidth, lvlHeight, Phaser.AUTO, '', { preload: preload, create: create, update: update });
}

function preload()
{
	game.load.image('jammu', './../../assets/Player1.png');
	game.load.image('jimmu', './../../assets/Player2.png');
	game.load.image('target', './../../assets/Target.png');
	game.load.image('spikes', './../../assets/Spikes.png');
	game.load.image('explosion', './../../assets/ExplosionParticle.png');
}


function create(){
	game.physics.startSystem(Phaser.Physics.ARCADE);
	game.stage.backgroundColor = '#339933';
	
	spikes = [];
	createSpikes();
	
	var huntedPlayerNumber = game.rnd.integerInRange(1,2);
	if(huntedPlayerNumber === 1){
		jammu = createPlayer('jammu', 1, speed, false, player1StartX, player1StartY);
		assignAsHunted(jammu);
		huntedPlayer = jammu;
		jimmu = createPlayer('target', 2, speed *1.5, false, 400, 300);
		jimmu.shoot = function(){ shootTarget(jimmu);};
	}
	else{
		jimmu = createPlayer('jimmu', 2, speed, false,  player2StartX, player2StartY);
		huntedPlayer = jimmu;
		jammu = createPlayer('target', 1, speed*1.5, false, 400, 300);
		jammu.shoot = function(){ shootTarget(jammu);};
	}
	
	
	
	//game.physics.arcade.enable(jimmu);
	//spikes.enableBody = true;
	//spikes.physicsBodyType = Phaser.Physics.ARCADE;

	game.time.events.add(Phaser.Timer.SECOND * startTime, start, this);
	
	
	player2Text = game.add.text(lvlWidth *0.1 -50, 50, "Game on!", { fontSize: '22px', fill: '#fff' });
	player1Text = game.add.text(lvlWidth *0.7 -50, 50, "Game on!", { fontSize: '22px', fill: '#fff' });
	startingTextComponent = game.add.text(lvlWidth *0.2-50, lvlHeight * 0.5 -10, startingText, { fontSize: '22px', fill: '#fff' });
}


function assignAsHunted(hunted)
{
	huntedPlayer = hunted;
	huntedPlayer.sprite = hunted.sprite;
	huntedPlayer.sprite.body = hunted.sprite.body;
}

function shootTarget(target){
	
	if( (target.sprite.body.x + target.sprite.body.halfWidth > huntedPlayer.sprite.body.x && target.sprite.body.x + target.sprite.body.halfWidth < huntedPlayer.sprite.body.x  + huntedPlayer.sprite.body.width )&&(
	target.sprite.body.y  + target.sprite.body.halfHeight  > huntedPlayer.sprite.body.y && target.sprite.body.y + target.sprite.body.halfHeight < huntedPlayer.sprite.body.y + huntedPlayer.sprite.body.height))
	{
		huntedPlayer.score--;
		//jimmu.score++;
		huntedPlayer.destroy();
		target.score++;
	}
	var explosion = game.add.sprite(target.sprite.body.x+target.sprite.body.halfWidth * 0.5 , target.sprite.body.y  + target.sprite.body.halfHeight* 0.5, 'explosion');
	game.time.events.add(Phaser.Timer.SECOND * startTime, function()
	{
		explosion.destroy();
	}, this);
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


function createPlayer(spriteName, playerKeyboardNumber, speed ,activity, x, y){
	var player = new Player(spriteName , game, speed, x, y, playerKeyboardNumber);
	player.setActivity(activity);
	player.registerPlayerAs(playerKeyboardNumber);
	return player;
}



function update(){
	if(game.time.totalElapsedSeconds() >= maxTime + startTime){	
		if(huntedPlayer.score ===0)
			huntedPlayer.score++;
		endGame();
	}
	for(var i=0; i < spikes.length; i++){
		game.physics.arcade.collide(huntedPlayer.sprite, spikes[i]);
	}


	
	player1Text.text = defineText(1);
	player2Text.text = defineText(2);
	jimmu.update(game);
	jammu.update(game);
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

function endGame(){
	if(gameEnded) return;
	gameEnded = true;
	game.add.text(lvlWidth *0.5 -50, lvlHeight * 0.5 -10, 'Game Over!', { fontSize: '22px', fill: '#fff' });
	
	jimmu.setActivity(false);
	jammu.setActivity(false);
	
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

