var jammu;
var jimmu;
var lvlWidth = 800;
var lvlHeight = 600;
var game = new Phaser.Game(lvlWidth, lvlHeight, Phaser.AUTO, '', { preload: preload, create: create, update: update });
var cursors;
var speed = 300;
//var maxScore = 5;
var maxTime = 20;
var startTime = 1;
var startingText = "Shoot your opponent!";
var startingTextComponent;
var player1Text;
var player2Text;
var amountOfSpikes = 5;
var spikes;
var player1StartX = 75;
var player2StartX = 725;
var balls = [];

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
	jammu = createPlayer('jammu', 1, false, 75, 100);
	jimmu = createPlayer('jimmu', 2, false, 725, 500);	
	jimmu.sprite.body.bounce.setTo(1,1);
	jammu.sprite.body.bounce.setTo(1,1);
	//game.physics.arcade.enable(jimmu);
	//spikes.enableBody = true;
	//spikes.physicsBodyType = Phaser.Physics.ARCADE;
	spikes = [];
	
	game.time.events.add(Phaser.Timer.SECOND * startTime, start, this);
	createSpikes();
	
	player1Text = game.add.text(lvlWidth *0.1 -50, 50, "P1 has collected "+jammu.score+" balls!", { fontSize: '22px', fill: '#fff' });
	player2Text = game.add.text(lvlWidth *0.65 -50, 50, "P2 has collected "+jimmu.score+" balls!", { fontSize: '22px', fill: '#fff' });
	startingTextComponent = game.add.text(lvlWidth *0.2-50, lvlHeight * 0.5 -10, startingText, { fontSize: '22px', fill: '#fff' });
}

function createSpikes(){
	for(var i = 0; i < amountOfSpikes; i++){
		var spike = game.add.sprite(game.rnd.integerInRange(player1StartX+50, player2StartX-50), game.rnd.integerInRange(0, 600), 'spikes');
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
	if(game.time.totalElapsedSeconds() >= maxTime + startTime) destroy();
	game.physics.arcade.collide(jimmu.sprite, jammu.sprite);
	for	(var i = 0; i < balls.length; i++){
		for(var j = 0; j < spikes.length; j++){
			game.physics.arcade.collide(spikes[j], balls[i],function(){
				spikes[j].destroy();
				balls[i].destroy();
			}, null, null)
		}
		game.physics.arcade.collide(jimmu.sprite, balls[i], function()
		{
			jimmu.sprite.destroy();
			jammu.score++;
		}, null, null);
		game.physics.arcade.collide(jammu.sprite, balls[i], function()
		{
			jammu.sprite.destroy();
			jimmu.score++;
		}, null, null);
		
	}


	
	player1Text.text = "P1 has collected "+jammu.score+" balls!";
	player2Text.text = "P2 has collected "+jimmu.score+" balls!";
	jimmu.update(game, balls);
	jammu.update(game, balls);
}


function destroy(){
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
}