var jammu;
var jimmu;
var lvlWidth = 800;
var lvlHeight = 600;
var game = new Phaser.Game(lvlWidth, lvlHeight, Phaser.AUTO, '', { preload: preload, create: create, update: update });
var cursors;
var speed = 200;
var maxScore = 5;
var maxTime = 10;
var startTime = 1;
var startingText = "Guide the other player to spikes to harm him!";
var startingTextComponent;
var amountOfSpikes = 30;
var player1Text;
var player2Text;
var spikes;

function preload()
{
	game.load.image('jammu', './../../assets/Player1.png');
	game.load.image('jimmu', './../../assets/Player2.png');
	game.load.image('spikes', './../../assets/Spikes.png');
}


function create(){
	game.physics.startSystem(Phaser.Physics.ARCADE);
	game.stage.backgroundColor = '#339933';
	jammu = createPlayer('jammu', 2, false, 200, 100);
	jimmu = createPlayer('jimmu', 1, false, 600, 100);	
	jimmu.sprite.body.bounce.setTo(1,1);
	jammu.sprite.body.bounce.setTo(1,1);
	//game.physics.arcade.enable(jimmu);
	spikes = [];
	//spikes.enableBody = true;
	//spikes.physicsBodyType = Phaser.Physics.ARCADE;

	game.time.events.add(Phaser.Timer.SECOND * startTime, start, this);
	for(var i = 0; i < amountOfSpikes; i++){
		var spike = game.add.sprite(game.rnd.integerInRange(0, 800), game.rnd.integerInRange(0, 600), 'spikes');
		game.physics.arcade.enable(spike);
		spike.body.immovable = true;
		spikes[spikes.length] = spike;
	}
	
	player1Text = game.add.text(lvlWidth *0.1 -50, 50, "P1 Hurt "+jammu.score+" times", { fontSize: '22px', fill: '#fff' });
	player2Text = game.add.text(lvlWidth *0.8 -50, 50, "P2 Hurt "+jimmu.score+" times", { fontSize: '22px', fill: '#fff' });
	startingTextComponent = game.add.text(lvlWidth *0.2-50, lvlHeight * 0.5 -10, startingText, { fontSize: '22px', fill: '#fff' });
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



function update(){
	if(game.time.totalElapsedSeconds() >= maxTime + startTime) endGame();
	game.physics.arcade.collide(jimmu.sprite, jammu.sprite);
	for(var i = 0; i < spikes.length; i++)
	{
		game.physics.arcade.collide(jimmu.sprite, spikes[i], function(){
			jimmu.score--;
			spikes[i].destroy();
		}, null, null);
		game.physics.arcade.collide(jammu.sprite, spikes[i], function(){
			jammu.score--;
			spikes[i].destroy();
		}, null, null);
	}


	
	player1Text.text = "P1 Hurt "+ -jammu.score+" times";
	player2Text.text = "P2 Hurt "+ -jimmu.score+" times";
	jimmu.update(game);
	jammu.update(game);
}


function endGame(){
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
