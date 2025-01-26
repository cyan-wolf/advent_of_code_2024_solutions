package code.advent;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.nio.file.Files;
import java.nio.file.Path;

public class Program {
	private record Inputs(ArrayList<int[]> startPositions, List<String> grid) {}
	
	// Reads the trail start positions and grid from the input file.
	private static Inputs readInputs() throws IOException {
		Path path = Path.of("./input.txt");
		List<String> lines = Files.readAllLines(path);
		
		ArrayList<int[]> startPositions = new ArrayList<>();
		
		for (int r = 0; r < lines.size(); r++) {
			for (int c = 0; c < lines.get(r).length(); c++) {
				if (lines.get(r).charAt(c) == '0') {
					startPositions.add(new int[] {r, c});
				}
			}
		}
		return new Inputs(startPositions, lines);
	}
	
	// Gets the height of the given position on the grid.
	// Returns -999 if the position is out of bounds.
	private static int getHeight(int[] pos, List<String> grid) {
		int row = pos[0];
		int col = pos[1];
		
		if (row >= 0 && row < grid.size() && col >= 0 && col < grid.getFirst().length()) {
			return grid.get(row).charAt(col) - '0';
		}
		return -999; // out of bounds
	}
	
	// Counts all the ways to reach the end of a trail.
	private static int countTrailPathsToEnd(int[] pos, List<String> grid) {
		final int currHeight = getHeight(pos, grid);
		
		final int[][] nbrs = {
			{pos[0], pos[1] + 1},
			{pos[0], pos[1] - 1},
			{pos[0] + 1, pos[1]},
			{pos[0] - 1, pos[1]},
		};
		
		int score = 0;
		
		for (final var nbrPos : nbrs) {
			final int nbrHeight = getHeight(nbrPos, grid);
			if (nbrHeight == currHeight + 1) {
				score += countTrailPathsToEnd(nbrPos, grid);
				
				if (nbrHeight == 9) {
					score += 1;
				}
			}
		}
		return score;
	}
	
	public static void main(String[] args) throws IOException {
		final var input = readInputs();
		
		int scoreSum = 0;
		
		for (final var startPos : input.startPositions) {
			scoreSum += countTrailPathsToEnd(startPos, input.grid);
		}
		System.out.println(scoreSum);
	}
}
