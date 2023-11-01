package game;

import java.awt.*;
import java.util.Set;

public class HexMatrix {
    private final Hex[][] hexes;

    int wi;
    int he;

    Set<Integer> born;
    Set<Integer> alive;

    public HexMatrix(int height, int width, int hexSize, Set<Integer> bo, Set<Integer> al) { // , Set<Integer> bornNum, Set<Integer> aliveNum
        he = height; wi = width;
        this.hexes = new Hex[he][wi];
        born = bo;
        alive = al;

        int startX = hexSize + 10;
        int startY = 10 + (int)(hexSize * 0.5 * 1.75);

        for(int y = 0; y < he;y++){
            for(int x = 0; x < wi; x++){
                hexes[y][x] = new Hex();
                if((x % 2) == 0){
                    hexes[y][x].initialSet((int)(hexSize * x * 1.5 + startX),(int)(hexSize * 1.75 * y + startY), hexSize );
                } else{
                    hexes[y][x].initialSet((int)(hexSize * x * 1.5 + startX),(int)(hexSize * 1.75 * y + startY + hexSize * 0.5 * 1.75),hexSize);
                }
            }
        }
    }
    public HexMatrix(int height, int width, int hexSize, Hex[][] hexmatrix, Set<Integer> bo, Set<Integer> al) { // , Set<Integer> bornNum, Set<Integer> aliveNum
        he = height; wi = width;
        this.hexes = hexmatrix;
        born = bo;
        alive = al;

        int startX = hexSize + 10;
        int startY = 10 + (int)(hexSize * 0.5 * 1.75);

        for(int y = 0; y < he;y++){
            for(int x = 0; x < wi; x++){;
                if((x % 2) == 0){
                    hexes[y][x].initialSet((int)(hexSize * x * 1.5 + startX),(int)(hexSize * 1.75 * y + startY), hexSize );
                } else{
                    hexes[y][x].initialSet((int)(hexSize * x * 1.5 + startX),(int)(hexSize * 1.75 * y + startY + hexSize * 0.5 * 1.75),hexSize);
                }
            }
        }

        //born = bornNum;
        //alive = aliveNum;

    }

    protected void paintComponent(Graphics g) {
        for (Hex[] row: hexes) {
            for (Hex hex: row) {
                hex.paintComponent(g);
            }
        }
    }


    public void refresh() {
        for(int i = 0; i < he; i++){
            for(int j = 0; i < wi; j++){
                int surroundings = getSurroundings(i, j);

                if(hexes[i][j].getState().equals(false) && born.contains(surroundings)){
                    hexes [i][j].setNewState(true);
                }
                if(hexes[i][j].getState().equals(true) && alive.contains(surroundings)){
                    hexes[i][j].setNewState(true);
                }
            }
        }
        for (Hex[] row: hexes){
            for(Hex hex: row){
                hex.update();
            }
        }
    }

    private int getSurroundings(int row, int col) {
        int surroundings = 0;

        if(Boolean.TRUE.equals(hexes[row - 1 % he][col].getState())){surroundings++;}           // above
        if(Boolean.TRUE.equals(hexes[row][col - 1 % wi].getState())){surroundings++;}  // upper left
        if(Boolean.TRUE.equals(hexes[row][col + 1 % wi].getState())){surroundings++;}  // upper right
        if(Boolean.TRUE.equals(hexes[row - 1 % he][col - 1 % wi].getState())){surroundings++;}           // lower left
        if(Boolean.TRUE.equals(hexes[row - 1 % he][col + 1 % wi].getState())){surroundings++;}           // lower right
        if(Boolean.TRUE.equals(hexes[row + 1 % he][col].getState())){surroundings++;}            // under
        return surroundings;
    }


}
