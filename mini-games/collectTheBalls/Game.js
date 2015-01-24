var jammu;
var jimmu;
var lvlWidth = 800;
var lvlHeight = 600;
var game = new Phaser.Game(lvlWidth, lvlHeight, Phaser.AUTO, '', { preload: preload, create: create, update: update });
var cursors;
var speed = 1000;
//var maxScore = 5;
var maxTime = 5;
var startTime = 1;
var startingText = "Collect more balls than your opponent!";
var startingTextComponent;
var player1Text;
var player2Text;
var ball;

function preload()
{
	game.load.image('jammu', './../../assets/Player1.png');
	game.load.image('jimmu', './../../assets/Player2.png');
	game.load.image('ball', './../../assets/Ball.png');
}


function create(){
	game.physics.startSystem(Phaser.Physics.ARCADE);
	game.stage.backgroundColor = '#339933';
	jammu = createPlayer('jammu', 1, false, 200, 100);
	jimmu = createPlayer('jimmu', 2, false, 600, 100);	
	jimmu.sprite.body.bounce.setTo(1,1);
	jammu.sprite.body.bounce.setTo(1,1);
	//game.physics.arcade.enable(jimmu);
	//spikes.enableBody = true;
	//spikes.physicsBodyType = Phaser.Physics.ARCADE;

	game.time.events.add(Phaser.Timer.SECOND * startTime, start, this);
	createRandomBall();
	
	player1Text = game.add.text(lvlWidth *0.65 -50, 50, "P1 has collected "+jammu.score+" balls!", { fontSize: '22px', fill: '#fff' });
	player2Text = game.add.text(lvlWidth *0.1 -50, 50, "P2 has collected "+jimmu.score+" balls!", { fontSize: '22px', fill: '#fff' });
	startingTextComponent = game.add.text(lvlWidth *0.2-50, lvlHeight * 0.5 -10, startingText, { fontSize: '22px', fill: '#fff' });
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



function update(){
	if(game.time.totalElapsedSeconds() >= maxTime + startTime) endGame();
	game.physics.arcade.collide(jimmu.sprite, jammu.sprite);
	game.physics.arcade.collide(jimmu.sprite, ball, function(){
		jimmu.score++;
		ball.destroy();
		createRandomBall();
	}, null, null);
	game.physics.arcade.collide(jammu.sprite, ball, function(){
		jammu.score++;
		ball.destroy();
		createRandomBall();
	}, null, null);



	
	player1Text.text = "P1 has collected "+jammu.score+" balls!";
	player2Text.text = "P2 has collected "+jimmu.score+" balls!";
	jimmu.update(game);
	jammu.update(game);
}


function endGame(){
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
		
	parent.$(parent.document).trigger("onGameEnd",result);
}


function destroy(gameResult){
	parent.$(parent.document).trigger("onGameEnd",gameResult);

}
