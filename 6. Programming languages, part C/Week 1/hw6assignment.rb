# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces + [
                 rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1, -1]]), #Shoe
                 [[[0, 0], [-1, 0], [1, 0], [-2, 0], [2, 0]], # xl-long (only needs two)
                 [[0, 0], [0, -1], [0, 1], [0, -2], [0, 2]]],
                 rotations([[0, 0], [0, 1], [1, 0]])] # hook

  # your enhancements here
  # class method to choose the next piece
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  # class method to select a cheat piece
  def self.cheat_piece (board)
    MyPiece.new([[[0, 0]]], board)
  end  
  
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block =  MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat_piece = 0
  end

  def use_cheat_piece
    if @score >= 100 and @cheat_piece == 0
      @cheat_piece += 1; @score -= 100
    end
  end

  # rotates the current piece counterclockwise
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  # gets the next piece
  def next_piece
    if @cheat_piece == 0
      @current_block = MyPiece.next_piece(self)
    else
      @current_block = MyPiece.cheat_piece(self)
      @cheat_piece = 0 
    end
    @current_pos = nil   
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    count = locations.size - 1
    (0..count).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
  
end

class MyTetris < Tetris
  # your enhancements here

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings  
    super 
    @root.bind('u', proc {@board.rotate_180}) 
    @root.bind('c', proc {@board.use_cheat_piece}) 
  end

end

