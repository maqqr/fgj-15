function Bird(game, x, y) {
	this.active = false;
	this.sheet = game.add.sprite(x, y, 'sheet');
  	this.sheet.animations.add('flap');
  	this.sheet.animations.play('flap', 12, true);
    this.active = false;
}

Bird.prototype.registerBirdAs = function(playerNumber){
	if(playerNumber == 1) {
		this.leftButton = game.input.keyboard.addKey("J".charCodeAt(0));
		this.rightButton = game.input.keyboard.addKey("L".charCodeAt(0));
		this.upButton = game.input.keyboard.addKey("I".charCodeAt(0));
		this.downButton = game.input.keyboard.addKey("K".charCodeAt(0));
		this.bButton = game.input.keyboard.addKey("N".charCodeAt(0));
		this.aButton = game.input.keyboard.addKey("M".charCodeAt(0));
	}
	else if (playerNumber == 2) {
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
  /*this.emitter = game.add.emitter(0, 0, 100);
    this.emitter.makeParticles('particle');
    this.emitter.gravity = 200;
	this.scoreText = game.add.text(this.spritebg.x + 144, this.spritebg.y + 170, '0', { fontSize: '50', fill: '#000'});
    this.emitter.x = this.scoreText.x;
    this.emitter.y = this.scoreText.y - 200;*/
}

Bird.prototype.update = function(game){
}


Bird.prototype.buttonPressed = function(button)
{
    this.sheet.body.velocity.y = -400;
}

Bird.prototype.addPhysics = function() {
	if(!this.active) {
    	game.physics.arcade.enableBody(this.sheet);
    	this.active = true;
	}
}