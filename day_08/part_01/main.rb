
# Read the input as a dictionary that 
# maps antenna frequencies to the positions of 
# the antennas with those frequencies.
# Also returns the amount of rows and columns in the map.
def read_input()
  antennas = {}
  row = 0
  col = nil

  File.foreach("input.txt") do |line| 
    line.strip!
    col = 0
    line.split("").each do |c|
      if c != "."
        if not antennas.has_key?(c)
          antennas[c] = []
        end
  
        antennas[c] = antennas[c].push([row, col])
      end
  
      col += 1
    end
    row += 1
  end

  return antennas, row, col
end

def in_bounds?(pos, rows, cols)
  r, c = pos
  return r >= 0 && c >= 0 && r < rows && c < cols
end

antennas, rows, cols = read_input()

# Store the antinodes in a hash set to avoid duplicates.
antinodes = {} # hash set

antennas.each_key do |a|
  # Check every antenna with the given frequency 'a'.
  antennas[a].each do |pos| 
    
    # Check every other antenna with the same frequency.
    antennas[a].each do |other_pos|  
      # Calculate the position of the antinode.
      dr = other_pos[0] - pos[0]
      dc = other_pos[1] - pos[1]
      anti_pos = [pos[0] - dr, pos[1] - dc]

      # Check if the antinode is valid.
      if pos != other_pos && in_bounds?(anti_pos, rows, cols)
        antinodes[anti_pos] = nil # hash set
      end
    end
  end
end

puts antinodes.length
