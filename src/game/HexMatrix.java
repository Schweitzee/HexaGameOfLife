package game;

import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.util.Set;
import java.util.TreeSet;


/**
 * Class representing the matrix of Hexes, it is a JPanel, so it can be drawn on the screen
 * It is also Serializable, so it can be saved and loaded
 * It is synchronized, so it can be used in multithreading for the simulation and drawing.
 * The threads are also synchronized by the paused Boolean, which is set to true when the simulation is paused.
 */
public class HexMatrix extends JPanel implements Serializable {

    /**
     * hexes is the matrix of Hexes.
     */
    private Hex[][] hexes;

    /**
     * Returns the matrix of Hexes.
     */
    private int wi;
    /**
     * wi is the width of the matrix.
     * @return width of the matrix as an Integer
     */
    public int getWi(){return wi;}

    /**
     * he is the height of the matrix.
     */
    private int he;

    /**
     * @return height of the matrix as an Integer
     */
    public int getHe() {return he;}

    /**
     * hexSize is the size of the hexagons.
     */
    private int hexSize;

    /**
     * @return size of the Hexes in pixels
     */
    public int getHexSize() {return hexSize;}

    /**
     * bornSet is the set of numbers, which make a Hex born.
     */
    TreeSet<Integer> bornSet = new TreeSet<>();

    /**
     * aliveSet is the set of numbers, which keep a Hex alive.
     */
    TreeSet<Integer> aliveSet = new TreeSet<>();

    /**
     * drawable is a Boolean, which is set to true when the simulation is ready to be drawn and set to false when the simulation is being calculated.
     */
    private Boolean drawable = true;

    /**
     * Sets the drawable Boolean to true.
     */
    public void setDrawableTrue() {drawable = true;}

    /**
     * paused is a Boolean, which is set to true when the simulation is paused.
     * By default it is set to true, so the simulation doesn't start immediately.
     */
    private Boolean paused = true;

    /**
     * @param paused Boolean to be set
     */
    public void setPaused(Boolean paused) {
        this.paused = paused;
    }

    /**
     * @return paused Boolean
     */
    public Boolean getPaused() {
        return paused;
    }
    /**
     *Notifies all threads, which are waiting on the HexMatrix object.
     */
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
     * Waits for the simulation to be ready to be drawn and then draws it.
     * @throws InterruptedException when the thread is interrupted
     */
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

    /**
     * Calls the draw method of every Hex in the matrix.
     * @param g the <code>Graphics</code> object to protect
     */
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


    /**
     * Calls the getSurroundings method for every Hex in the matrix and based on the result sets the new state of the Hex.
     * Synced with the drawing method, so it can't be called while the drawing method is running.
     * @throws InterruptedException when the thread is interrupted
     */
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

    /**
     * Returns the number of neighbours of a Hex.
     * @param row row of the Hex
     * @param col column of the Hex
     * @return number of neighbours of the Hex
     */
    public int getSurroundings(int row, int col) {
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

    /**
     * @return the set of numbers, which make a Hex born as an Integer.
     */
    public int getBornSet(){
        int res = 0;
        for (int i:bornSet) {
            res = res*10;
            res += i;
        }
        return res;
    }

    /**
     * @return the set of numbers, which keep a Hex alive as an Integer.
     */
    public int getAliveSet(){
        int res = 0;
        for (int i:aliveSet) {
            res = res*10;
            res += i;
        }
        return res;
    }

    /**
     * @return the matrix of Hexes.
     */
    public Hex[][] getHexes() {
        return hexes;
    }

    /**
     * DeSerializes the HexMatrix object.
     * @param in the input stream
                 * @throws IOException when the stream is corrupted
                 * @throws ClassNotFoundException when the class of the object is not found
     */
    @Serial
    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
        wi = (int) in.readObject();
        he = (int) in.readObject();
        hexSize = (int) in.readObject();
        bornSet = (TreeSet<Integer>) (in.readObject());
        aliveSet = (TreeSet<Integer>) (in.readObject());
        hexes = new Hex[he][wi];

        int startX = hexSize;
        int startY =  (int)(hexSize * 0.9);

        for (int y = 0; y < he; y++) {
            for (int x = 0; x < wi; x++) {
                hexes[y][x] = new Hex((Boolean) in.readObject());
                if((x % 2) == 0){
                    hexes[y][x].initialSet((int)(hexSize * x * 1.5 + startX),(int)(hexSize * 1.75 * y + startY), hexSize );
                } else{
                    hexes[y][x].initialSet((int)(hexSize * x * 1.5 + startX),(int)(hexSize * 1.75 * y + startY + hexSize * 0.5 * 1.75),hexSize);
                }
            }
        }
        drawable = true;
        paused = true;
    }


    /**
     * Serializes the HexMatrix object to the output stream.
     * @param out the output stream
     *            @throws IOException when the stream is corrupted
     */
    @Serial
    private void writeObject(ObjectOutputStream out) throws IOException {
        out.writeObject(wi);
        out.writeObject(he);
        out.writeObject(hexSize);
        out.writeObject(bornSet);
        out.writeObject(aliveSet);
        for (Hex[] hexRow:hexes) {
            for (Hex hex:hexRow) {
                out.writeObject(hex.getState());
            }
        }
    }
}
