package game;

import javax.swing.*;
import java.awt.*;
import java.util.Set;
import java.util.TreeSet;

public class HexMatrix extends JPanel{
    private final transient Hex[][] hexes;
    private final int wi;
    public int getWi(){return wi;}
    private final int he;
    public int getHe() {return he;}
    private final int hexSize;
    public int getHexSize() {return hexSize;}
    TreeSet<Integer> bornSet = new TreeSet<>();
    TreeSet<Integer> aliveSet = new TreeSet<>();
    private Boolean drawable = true;
    public void setDrawableTrue() {drawable = true;}
    private Boolean paused = true;
    public void setPaused(Boolean paused) {
        this.paused = paused;
    }
    public Boolean getPaused() {
        return paused;
    }
    public synchronized void noti(){this.notifyAll();}


    /**
     * Constructor based on only parameters of the new simulation, no starting data for the of the Hexes
     * @param height height of the simulation matrix
     * @param width width of the simulation matrix
     * @param hexSize size of the hexes in pixels, read documentation to see which size
     * @param bo Set storing Integers, these numbers make a Hex born
     * @param al Set storing Integers, these numbers keep a Hex alive
     */
    public HexMatrix(int height, int width, int hexSize, Set<Integer> bo, Set<Integer> al) {
        he = height; wi = width;
        this.hexes = new Hex[he][wi];
        this.hexSize = hexSize;
        bornSet.addAll(bo);
        aliveSet.addAll(al);

        int startX = hexSize;
        int startY =  (int)(hexSize * 0.9);

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

    /**
     * Constructor based on input from a file, Hexes in hexMatrix can have differing states
     * @param height height of the simulation matrix
     * @param width width of the simulation matrix
     * @param hexSize size of the hexes in pixels, read documentation to see which size
     * @param hexMatrix initialized array of Hexes, with states alive or dead
     * @param bo Set storing Integers, these numbers make a Hex born
     * @param al Set storing Integers, these numbers keep a Hex alive
     */
    public HexMatrix(int height, int width, int hexSize, Hex[][] hexMatrix, Set<Integer> bo, Set<Integer> al) {
        he = height; wi = width;
        this.hexSize = hexSize;
        this.hexes = hexMatrix;
        bornSet.addAll(bo);
        aliveSet.addAll(al);

        int startX = hexSize;
        int startY = hexSize;

        for(int y = 0; y < he;y++){
            for(int x = 0; x < wi; x++){
                if((x % 2) == 0){
                    hexes[y][x].initialSet((int)(hexSize * x * 1.5 + startX),(int)(hexSize * 1.75 * y + startY), hexSize );
                } else{
                    hexes[y][x].initialSet((int)(hexSize * x * 1.5 + startX),(int)(hexSize * 1.75 * y + startY + hexSize * 0.5 * 1.75),hexSize);
                }
            }
        }
    }

    protected synchronized void syncedPaint() throws InterruptedException {

        while(Boolean.TRUE.equals(paused)){
            this.wait();
        }

        while(Boolean.FALSE.equals(drawable)){
            this.wait();
        }

        this.repaint();

        drawable = false;
        this.notifyAll();
    }

    @Override
    public void paintComponent(Graphics g){
        super.repaint();
        if(hexes != null) {
            for (Hex[] row : hexes) {
                for (Hex hex : row) {
                    hex.draw(g);
                }
            }
        }
    }



    public synchronized void refresh() throws InterruptedException {

        while(Boolean.TRUE.equals(paused)){
            this.wait();
        }

        while(Boolean.TRUE.equals(drawable)){
            this.wait();
        }
        for(int i = 0; i < he; i++){
            for(int j = 0; j < wi; j++){
                int surroundings = getSurroundings(i, j);

                if(hexes[i][j].getState().equals(false) && bornSet.contains(surroundings)){
                    hexes [i][j].setNewState(true);
                }else hexes[i][j].setNewState(hexes[i][j].getState().equals(true) && aliveSet.contains(surroundings));

            }
        }
        for (Hex[] row: hexes){
            for(Hex hex: row){
                hex.switchStateToNewState();
            }
        }
        drawable = true;
        this.notifyAll();
    }

    private int getSurroundings(int row, int col) {
        int surroundings = 0;

        if(Boolean.TRUE.equals(hexes[Math.floorMod(row-1, he)][col].getState())){surroundings++;}           // above
        if(Boolean.TRUE.equals(hexes[Math.floorMod(row+1, he)][col].getState())){surroundings++;}            // under

        if(col % 2 == 1) {      //if lowered column
            if (Boolean.TRUE.equals(hexes[row][Math.floorMod(col - 1, wi)].getState())) {surroundings++;}           // upper left
            if (Boolean.TRUE.equals(hexes[row][Math.floorMod(col + 1, wi)].getState())) {surroundings++;}           // upper right

            if (Boolean.TRUE.equals(hexes[Math.floorMod(row+1,he)][Math.floorMod(col - 1, wi)].getState())) {surroundings++;}           // lower left
            if (Boolean.TRUE.equals(hexes[Math.floorMod(row+1,he)][Math.floorMod(col + 1, wi)].getState())) {surroundings++;}           // lower right
        }else{              //if raised column
            if (Boolean.TRUE.equals(hexes[Math.floorMod(row-1,he)][Math.floorMod(col - 1, wi)].getState())) {surroundings++;}           // upper left
            if (Boolean.TRUE.equals(hexes[Math.floorMod(row-1,he)][Math.floorMod(col + 1, wi)].getState())) {surroundings++;}           // upper right

            if (Boolean.TRUE.equals(hexes[row][Math.floorMod(col - 1, wi)].getState())) {surroundings++;}           // lower left
            if (Boolean.TRUE.equals(hexes[row][Math.floorMod(col + 1, wi)].getState())) {surroundings++;}           // lower right
        }
        return surroundings;
    }

    public int getBornSet(){
        int res = 0;
        for (int i:bornSet) {
            res = res*10;
            res += i;
        }
        return res;
    }

    public int getAliveSet(){
        int res = 0;
        for (int i:aliveSet) {
            res = res*10;
            res += i;
        }
        return res;
    }

    public Hex[][] getHexes() {
        return hexes;
    }

}
