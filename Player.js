function Player(left, right, up, down, sprite, game, speed){
	this.left = left;
	this.right = right;
	this.up = up;
	this.down = down;
	this.speed = speed;
	this.sprite = game.add.sprite(100, 100, sprite)
	game.physics.arcade.enable(this.sprite);
	
}

Player.prototype.update = function(game){

	if(game.input.keyboard.isDown(this.left))
	{
		move(-this.speed, 0, this.sprite);
	}
	else if(game.input.keyboard.isDown(this.right))
	{
		move(this.speed, 0, this.sprite);
	}
	else if(game.input.keyboard.isDown(this.down))
	{
		move(0, this.speed, this.sprite);
	}
	else if(game.input.keyboard.isDown(this.up))
	{
		move(0, -this.speed, this.sprite);
	}
	else
	{
		this.sprite.body.velocity.x = 0;
		this.sprite.body.velocity.y = 0;
	}
}

function moveWithinField(xToMove, yToMove, characterObject){
	if(characterObject.body.x < 0 + characterObject.body.halfWidth)
		characterObject.body.x += lvlWidth;
	else if(characterObject.body.y < 0 - characterObject.body.halfHeight)
		characterObject.body.y += lvlHeight;
	else if(characterObject.body.y > lvlHeight - characterObject.body.halfHeight)
		characterObject.body.y += - lvlHeight;
	else if(characterObject.body.x > lvlWidth - characterObject.body.halfWidth)
		characterObject.body.x += -lvlWidth;
	move(xToMove, yToMove, characterObject);
}

function move(xToMove, yToMove, characterObject){
	characterObject.body.x += xToMove;
	characterObject.body.y += yToMove;
}

