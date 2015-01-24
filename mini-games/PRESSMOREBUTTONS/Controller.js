var tintCol = 0xaaaaaa;

function Controller(game, x, y) {
	this.spritebg = game.add.sprite(x, y, 'controller');
	this.spriteup = game.add.sprite(x, y, 'up');
	this.spritedown = game.add.sprite(x, y, 'down');
	this.spriteleft = game.add.sprite(x, y, 'left');
	this.spriteright = game.add.sprite(x, y, 'right');
	this.spritea = game.add.sprite(x, y, 'a');
	this.spriteb = game.add.sprite(x, y, 'b');
	this.score = 0;
	this.active = false;
}

Controller.prototype.registerControllerAs = function(playerNumber){
	if(playerNumber == 1) {
		this.left = "J".charCodeAt(0);
		this.right = "L".charCodeAt(0);
		this.up = "I".charCodeAt(0);
		this.down = "K".charCodeAt(0);
		this.b = "N".charCodeAt(0);
		this.a = "M".charCodeAt(0);

		this.leftButton = game.input.keyboard.addKey("J".charCodeAt(0));
		this.rightButton = game.input.keyboard.addKey("L".charCodeAt(0));
		this.upButton = game.input.keyboard.addKey("I".charCodeAt(0));
		this.downButton = game.input.keyboard.addKey("K".charCodeAt(0));
		this.bButton = game.input.keyboard.addKey("N".charCodeAt(0));
		this.aButton = game.input.keyboard.addKey("M".charCodeAt(0));
	}
	else if (playerNumber == 2) {
		this.left = "S".charCodeAt(0);
		this.right = "F".charCodeAt(0);
		this.up = "E".charCodeAt(0);
		this.down = "D".charCodeAt(0);
		this.a = "A".charCodeAt(0);
		this.b = "Q".charCodeAt(0);

		this.leftButton = game.input.keyboard.addKey("S".charCodeAt(0));
		this.rightButton = game.input.keyboard.addKey("F".charCodeAt(0));
		this.upButton = game.input.keyboard.addKey("E".charCodeAt(0));
		this.downButton = game.input.keyboard.addKey("D".charCodeAt(0));
		this.bButton = game.input.keyboard.addKey("Q".charCodeAt(0));
		this.aButton = game.input.keyboard.addKey("A".charCodeAt(0));
	}

	this.leftButton.onDown.add(function () { this.buttonPressed(3); }, this);
	this.rightButton.onDown.add(function () { this.buttonPressed(4); }, this);
	this.upButton.onDown.add(function () { this.buttonPressed(1); }, this);
	this.downButton.onDown.add(function () { this.buttonPressed(2); }, this);
	this.bButton.onDown.add(function () { this.buttonPressed(6); }, this);
	this.aButton.onDown.add(function () { this.buttonPressed(5); }, this);
  	this.emitter = game.add.emitter(0, 0, 100);
    this.emitter.makeParticles('particle');
    this.emitter.gravity = 200;
	this.scoreText = game.add.text(this.spritebg.x + 144, this.spritebg.y + 170, '0', { fontSize: '50', fill: '#000'});
    this.emitter.x = this.scoreText.x;
    this.emitter.y = this.scoreText.y - 200;
}

Controller.prototype.update = function(game){
	var pressed = -1;
	if(game.input.keyboard.isDown(this.left)){
		this.spriteleft.tint = tintCol;
		pressed = 3;
	}
	else
		this.spriteleft.tint = 0xffffff;
	
	if(game.input.keyboard.isDown(this.right)){
		this.spriteright.tint = tintCol;
		pressed = 4;
	}
	else
		this.spriteright.tint = 0xffffff;

	if(game.input.keyboard.isDown(this.up)){
		this.spriteup.tint = tintCol;
		pressed = 1;
	}
	else
		this.spriteup.tint = 0xffffff;

	if(game.input.keyboard.isDown(this.down)){
		this.spritedown.tint = tintCol;
		pressed = 2;
	}
	else
		this.spritedown.tint = 0xffffff;

	if(game.input.keyboard.isDown(this.a)){
		this.spritea.tint = 0x00ff00;
		pressed = 5;
	}
	else
		this.spritea.tint = 0xffffff;

	if(game.input.keyboard.isDown(this.b)){
		this.spriteb.tint = 0xff0000;
		pressed = 6;
	}
	else
		this.spriteb.tint = 0xffffff;

	
	this.scoreText.text = this.score;
}


Controller.prototype.buttonPressed = function(button)
{
	if (this.active)
	{
		this.score++;
		this.emitter.start(true, 2000, null, 1);
	}
}