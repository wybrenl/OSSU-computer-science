# University of Washington, Programming Languages, Homework 7, hw7.rb 
# (See also ML code)

# a little language for 2D geometry objects

# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for 
#       for being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by 
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction

# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

class GeometryExpression  
    # do *not* change this class definition
    Epsilon = 0.00001
  end
  
  class GeometryValue 
    # do *not* change methods in this class definition
    # you can add methods if you wish
  
    private
    # some helper methods that may be generally useful
    def real_close(r1,r2) 
      (r1 - r2).abs < GeometryExpression::Epsilon
    end
    def real_close_point(x1,y1,x2,y2) 
      real_close(x1,x2) && real_close(y1,y2)
    end
    # two_points_to_line could return a Line or a VerticalLine
    def two_points_to_line(x1,y1,x2,y2) 
      if real_close(x1,x2)
        VerticalLine.new x1
      else
        m = (y2 - y1).to_f / (x2 - x1)
        b = y1 - m * x1
        Line.new(m,b)
      end
    end

    def inbetween(v,end1,end2)
		#(end1 - Epsilon <= v and v <= end2 + Epsilon) or (end2 - Epsilon <= v and v <= end1 + Epsilon)
        (end1 - 0.0001 <= v and v <= end2 + 0.0001) or (end2 - 0.0001 <= v and v <= end1 + 0.0001)
    end
  
    public
    # we put this in this class so all subclasses can inherit it:
    # the intersection of self with a NoPoints is a NoPoints object
    def intersectNoPoints np
      np # could also have NoPoints.new here instead
    end
  
    # we put this in this class so all subclasses can inhert it:
    # the intersection of self with a LineSegment is computed by
    # first intersecting with the line containing the segment and then
    # calling the result's intersectWithSegmentAsLineResult with the segment
    def intersectLineSegment seg
      line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
      line_result.intersectWithSegmentAsLineResult seg
    end

    def eval_prog env 
        self # all values evaluate to self
    end

    def preprocess_prog
        self # no pre-processing to do here
    end
  end
  
  class NoPoints < GeometryValue
    # do *not* change this class definition: everything is done for you
    # (although this is the easiest class, it shows what methods every subclass
    # of geometry values needs)
    # However, you *may* move methods from here to a superclass if you wish to
  
    # Note: no initialize method only because there is nothing it needs to do

    def shift(dx,dy)
      self # shifting no-points is no-points
    end
    def intersect other
      other.intersectNoPoints self # will be NoPoints but follow double-dispatch
    end
    def intersectPoint p
      self # intersection with point and no-points is no-points
    end
    def intersectLine line
      self # intersection with line and no-points is no-points
    end
    def intersectVerticalLine vline
      self # intersection with line and no-points is no-points
    end
    # if self is the intersection of (1) some shape s and (2) 
    # the line containing seg, then we return the intersection of the 
    # shape s and the seg.  seg is an instance of LineSegment
    def intersectWithSegmentAsLineResult seg
      self
    end
  end
  
  
  class Point < GeometryValue
    # *add* methods to this class -- do *not* change given code and do not
    # override any methods
  
    # Note: You may want a private helper method like the local
    # helper function inbetween in the ML code
    attr_reader :x, :y
    def initialize(x,y)
      @x = x
      @y = y
    end

    def shift(dx,dy)
        Point.new(x+dx,y+dy)
    end
    #1st dispatch
    def intersect other
        other.intersectPoint self
    end
    #2nd dispatch
    def intersectPoint p
        if real_close_point(@x, @y, p.x, p.y) then self else NoPoints.new end                
    end

    def intersectLine line
        if real_close(@y, @x*line.m + line.b) then self else NoPoints.new end    
    end

    def intersectVerticalLine vline
        if real_close(@x,vline.x) then self else NoPoints.new end
    end

    def intersectWithSegmentAsLineResult seg
        x = inbetween(@x,seg.x1,seg.x2)
        y = inbetween(@y,seg.y1,seg.y2)
        if x && y then self else NoPoints.new end 
    end
  end

  class Line < GeometryValue
    # *add* methods to this class -- do *not* change given code and do not
    # override any methods
    attr_reader :m, :b 
    def initialize(m,b)
      @m = m
      @b = b
    end
    def shift(dx,dy)
        Line.new(m,(b+dy-m*dx))
    end
    #1st dispatch
    def intersect other
        other.intersectLine self
    end
    #2nd dispatch
    def intersectPoint point
        point.intersectLine self            
    end

    def intersectLine line
        if real_close(@m, line.m)
            if real_close(@b, line.b)
                self #same line
            else
                NoPoints.new #parallel lines at different heights
            end
        else
            x = (line.b - @b) / (@m - line.m)
            y = @m*x+@b
            Point.new(x,y)
        end
    end

    def intersectVerticalLine vline
        Point.new(vline.x, @m*vline.x+@b)
    end

    def intersectWithSegmentAsLineResult seg
        seg
    end
  end
  
  class VerticalLine < GeometryValue
    # *add* methods to this class -- do *not* change given code and do not
    # override any methods
    attr_reader :x
    def initialize x
      @x = x
    end
    def shift(dx,dy)
        VerticalLine.new(x+dx)
    end
    #1st dispatch
    def intersect other
        other.intersectVerticalLine self
    end
    #2nd dispatch
    def intersectPoint point
        point.intersectVerticalLine self            
    end

    def intersectLine line
        line.intersectVerticalLine self
    end

    def intersectVerticalLine vline
        if real_close(@x, vline.x) then self else NoPoints.new end 
    end

    def intersectWithSegmentAsLineResult seg
        seg
    end
  end
  
  class LineSegment < GeometryValue
    # *add* methods to this class -- do *not* change given code and do not
    # override any methods
    # Note: This is the most difficult class.  In the sample solution,
    #  preprocess_prog is about 15 lines long and 
    # intersectWithSegmentAsLineResult is about 40 lines long
    attr_reader :x1, :y1, :x2, :y2
    def initialize (x1,y1,x2,y2)
      @x1 = x1
      @y1 = y1
      @x2 = x2
      @y2 = y2
    end

    def preprocess_prog
        if real_close_point(@x1,@y1,@x2,@y2)
          Point.new(@x1,@y1)
        elsif real_close(@x1,@x2)
          if @y1 > @y2
            LineSegment.new(@x2,@y2,@x1,@y1)
          else 
            self
          end
        elsif @x1 > @x2
          LineSegment.new(@x2,@y2,@x1,@y1)
        else
          self
        end
      end
    
    def shift(dx,dy)
        LineSegment.new(x1+dx,y1+dy,x2+dx,y2+dy)
    end
    #1st dispatch
    def intersect other
        other.intersectLineSegment self
    end
    #2nd dispatch
    def intersectPoint point
        point.intersectWithSegmentAsLineResult self            
    end

    def intersectLine line
        line.intersectWithSegmentAsLineResult self
    end

    def intersectVerticalLine vline
        vline.intersectWithSegmentAsLineResult self
    end

    def intersectWithSegmentAsLineResult seg
        if real_close(@x1,@x2) #if true then linesegments are on a vertical line
            if @y1 <= seg.y1 #case where 1st line starts below second line
                if real_close(@y2,seg.y1) 
                    Point.new(@x2,seg.y1) # just touching
                elsif @y2 < seg.y1 
                    NoPoints.new # disjoint
                elsif @y2 > seg.y2 
                    LineSegment.new(seg.x1,seg.y1,seg.x2,seg.y2) #b inside a
                else 
                    LineSegment.new(seg.x1,seg.y1,@x2,@y2)
                end
            else 
                if real_close(@y1,seg.y2) 
                    Point.new(@x2,@y1) # just touching
                elsif @y1 < seg.y2 
                    NoPoints.new # disjoint
                elsif @y1 > seg.y1 
                    LineSegment.new(@x1,@y1,@x2,@y2) #a inside b
                else 
                    LineSegment.new(@x1,@y1,seg.x2,seg.y2) #overlapping
                end
            end
        else
            if @x1 <= seg.x1 #case where 1st line starts before the second line
                if real_close(@x2,seg.x1) 
                    Point.new(@x2,@y2) # just touching
                elsif @x2 < seg.x1 
                    NoPoints.new # disjoint
                elsif @x2 > seg.x2 
                    LineSegment.new(seg.x1,seg.y1,seg.x2,seg.y2) #b inside a
                else 
                    LineSegment.new(seg.x1,seg.y1,@x2,@y2) #overlapping
                end
            else #case where 1st line starts after the second line
                if real_close(@x1,seg.x2)
                    Point.new(@x1,@y1) # just touching
                elsif @x1 < seg.x2
                    NoPoints.new # disjoint
                elsif @x1 > seg.x1
                    LineSegment.new(@x1,@y1,@x2,@y2) #b inside a
                else 
                    LineSegment.new(@x1,@y1,seg.x2,seg.y2) #overlapping
                end
            end            
        end
    end 
  end
  
  # Note: there is no need for getter methods for the non-value classes
  
  class Intersect < GeometryExpression
    # *add* methods to this class -- do *not* change given code and do not
    # override any methods
    def initialize(e1,e2)
      @e1 = e1
      @e2 = e2
    end
    def preprocess_prog
        Intersect.new(@e1.preprocess_prog, @e2.preprocess_prog)
    end
    def eval_prog env
        @e1.eval_prog(env).intersect @e2.eval_prog(env)
    end
  end
  
  class Let < GeometryExpression
    # *add* methods to this class -- do *not* change given code and do not
    # override any methods
    # Note: Look at Var to guide how you implement Let
    def initialize(s,e1,e2)
      @s = s
      @e1 = e1
      @e2 = e2
    end

    def preprocess_prog
        Let.new(@s,@e1.preprocess_prog,@e2.preprocess_prog)
    end

    def eval_prog env
        @e2.eval_prog([[@s,(@e1.eval_prog env)]] + env)
    end
  end
  
  class Var < GeometryExpression
    # *add* methods to this class -- do *not* change given code and do not
    # override any methods
    def initialize s
      @s = s
    end
    def eval_prog env # remember: do not change this method
      pr = env.assoc @s
      raise "undefined variable" if pr.nil?
      pr[1]
    end
    def preprocess_prog
        self
    end
  end
  
  class Shift < GeometryExpression
    # *add* methods to this class -- do *not* change given code and do not
    # override any methods
    def initialize(dx,dy,e)
      @dx = dx
      @dy = dy
      @e = e
    end
    def preprocess_prog
        Shift.new(@dx,@dy,@e.preprocess_prog)
    end   
    def eval_prog env
        @e.eval_prog(env).shift(@dx,@dy)
    end
  end