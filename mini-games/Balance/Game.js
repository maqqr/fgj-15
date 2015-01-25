var jammu;
var jimmu;
var lvlWidth = 800;
var lvlHeight = 600;
var game;
var speed = 300;
//var maxScore = 5;
var maxTime = 30;
var startTime = 1;
var startingText = "Keep your balance!";
var startingTextComponent;
var player1Text;
var player2Text;
var angleMultiplier = 1.2;
var player2StartX = 250;
var player2StartY = 300;
var player1StartX = 550;
var player1StartY = 300;
var gameEnded = false;
init();

function init(){
	game = new Phaser.Game(lvlWidth, lvlHeight, Phaser.AUTO, '', { preload: preload, create: create, update: update });
}

function preload()
{

	game.load.image('jammu', './../../assets/Player1.png');
	game.load.image('jimmu', './../../assets/Player2.png');
	game.load.image('ball', './../../assets/Ball.png');
	game.load.image('blueBall', './../../assets/BallBlue.png');
}


function create(){
	game.physics.startSystem(Phaser.Physics.ARCADE);
	game.stage.backgroundColor = '#339933';
	
	jammu = createPlayer('jammu', 1, speed, false, player1StartX, player1StartY);
	jimmu = createPlayer('jimmu', 2, speed, false, player2StartX, player2StartY);
	game.add.sprite(player1StartX - jimmu.sprite.body.halfWidth, player1StartY , 'ball');
	game.add.sprite(player2StartX - jimmu.sprite.body.halfWidth, player2StartY, 'blueBall');
	
	game.time.events.add(Phaser.Timer.SECOND * startTime, start, this);
	
	player2Text = game.add.text(lvlWidth *0.1 -50, 50, "Game on!", { fontSize: '22px', fill: '#fff' });
	player1Text = game.add.text(lvlWidth *0.7 -50, 50, "Game on!", { fontSize: '22px', fill: '#fff' });
	startingTextComponent = game.add.text(lvlWidth *0.2-50, lvlHeight * 0.5 -10, startingText, { fontSize: '22px', fill: '#fff' });
}

function start(){
	var chance = game.rnd.integerInRange(1, 2);
	jimmu.setActivity(true);
	jimmu.sprite.body.angularVelocity += chance === 1 ? -2 : 2;
	jammu.setActivity(true);
	var chance = game.rnd.integerInRange(1, 2);
	jammu.sprite.body.angularVelocity += chance === 1 ? -2 : 2;
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
		endGame();
	}
	
	//var chance = game.rnd.integerInRange(0, 100);
	//if(chance === 0 || chance == 1)
	//{
	//	angleMultiplier *= -1;
	//}
	//else{
		angleMultiplier *= 1.001;
	//}
	jimmu.update(game, 4,angleMultiplier);
	jammu.update(game, 4,angleMultiplier);
	
	if(Math.abs(jimmu.sprite.angle) > 90)
	{
		jimmu.destroy();
		jimmu.score--;
		jammu.score++;
		endGame();
	}
	if(Math.abs(jammu.sprite.angle) > 90)
	{
		jammu.destroy();
		jammu.score--;
		jimmu.score++;
		endGame();
	}
	
	player1Text.text = defineText(1);
	player2Text.text = defineText(2);
	
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
		
	game.time.events.add(Phaser.Timer.SECOND * 1, function(){
		destroy(result);	
	}, this);
}


function destroy(gameResult){
	parent.$(parent.document).trigger("onGameEnd",gameResult);
	game.destroy();

}

