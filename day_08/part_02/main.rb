
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

antennas.each_key do |freq|
  # Check every antenna with the given frequency.
  antennas[freq].each do |pos| 
    # If there is more than one antenna with the same 
    # frequency, then we know that there is an antinode 
    # at every antenna.
    if antennas[freq].length > 1 
      antinodes[pos] = nil
    end

    # Check every other antenna with the same frequency.
    antennas[freq].each do |other_pos|  
      # Check if the current we're looking at a different position.
      if pos != other_pos
        # Calculate the position of the antinode.
        dr = other_pos[0] - pos[0]
        dc = other_pos[1] - pos[1]

        # 'Normalize' the differences by dividing
        # by the GCD of both differences. This ensures
        # that every gridline is visited on the same line.
        gcd = dr.gcd(dc)
        dr = dr / gcd
        dc = dc / gcd

        # Calcuate the first antinode.
        anti_pos = [pos[0] - dr, pos[1] - dc]

        # Generate all antinodes along this same "line".
        while in_bounds?(anti_pos, rows, cols) do
          antinodes[anti_pos] = nil # hash set

          # Calculate the next antinode.
          anti_pos = [anti_pos[0] - dr, anti_pos[1] - dc]
        end
      end
    end
  end
end

puts antinodes.length
