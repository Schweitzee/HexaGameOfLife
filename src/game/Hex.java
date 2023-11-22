package game;

import java.awt.*;
import java.io.IOException;
import java.io.*;

/**
 * The hexagon class represents a hexagon on the game board.
 * Can be constructed with or without a state. (State is false by default)
 * The hexaqons are stored in a matrix in the HexMatrix class.
 * The current state of the hexagon can't be changed directly, only the new state can be set.
 */
public class Hex implements Serializable{

    /**
     * The state of the hexagon.
     * false = dead, true = alive
     */
    private Boolean state = false;

    /**
     * The new state of the hexagon.
     */
    private transient Boolean newState = false;

    /**
     * The x coordinates of the hexagon's vertices.
     */
    private final transient int[] xList = new int[6];

    /**
     * The y coordinates of the hexagon's vertices.
     */
    private final transient int[] yList = new int[6];

    /**
     * Constructor for the hexagon class.
     */
    public Hex(){}

    /**
     * Constructor for the hexagon class.
     * @param st The state of the hexagon.
     */
    public Hex(Boolean st){
        state = st;
    }

    /**
     * Sets the initial coordinates of the hexagon.
     * @param x The x coordinate of the center of the hexagon.
     * @param y The y coordinate of the center of the hexagon.
     * @param size The size of the hexagon.
     */
    public void initialSet(int x, int y, int size){

        xList[0] = x - (int)(size*0.5);
        xList[1] = x + (int)(size*0.5);
        xList[2] = x + size;
        xList[3] = x + (int)(size*0.5);
        xList [4] = x - (int)(size*0.5);
        xList[5] = x - size;

        yList[0] = (int) (y - (1.75 * size * 0.5));
        yList[1] = (int) (y - (1.75 * size * 0.5));
        yList[2] = y;
        yList[3] = (int) (y + (1.75 * size * 0.5));
        yList[4] = (int) (y + (1.75 * size * 0.5));
        yList[5] = y;
    }


    /**
     * Draws the hexagon.
     * @param g The graphics object to draw on.
     */
    public void draw(Graphics g) {
        Polygon hexagon = new Polygon();
        hexagon.npoints = 6;
        hexagon.xpoints = xList;
        hexagon.ypoints = yList;

        Graphics2D g2d = (Graphics2D) g;
        g2d.setColor(Color.YELLOW);
        g2d.draw(hexagon);
        if (Boolean.TRUE.equals(this.state)){
            g2d.setColor(Color.PINK);
            g2d.fill(hexagon);
        }else {
            g2d.setColor(Color.BLACK);
            g2d.fill(hexagon);
        }
    }


    /**
     * Refreshes the state of the hexagon.
     */
    public void switchStateToNewState() { state = newState; }

    /**
     * Getter for the state of the hexagon.
     * @return Boolean The state of the hexagon.
     */
    public Boolean getState(){
        return state;
    }

    /**
     * Sets the new state of the hexagon.
     * @param newSt The new state of the hexagon.
     */
    public void setNewState(Boolean newSt) {
        newState = newSt;
    }


    /**
     * Checks if the given coordinates are inside the hexagon. (used for the click event)
     * @param x The x coordinate of the point.
     * @param y The y coordinate of the point.
     * @return Boolean True if the given coordinates are inside the hexagon.
     */
    public Boolean contains(int x, int y) {
        Polygon hexagon = new Polygon();
        hexagon.npoints = 6;
        hexagon.xpoints = xList;
        hexagon.ypoints = yList;
        return hexagon.contains(x,y);
    }

    /**
     * Reads the object from a serialized stream.
     * @param in The input stream to read from.
     *           @throws IOException If there is an error reading from the stream.
     *           @throws ClassNotFoundException If the stream does not contain a valid Hex object.
     */
    @Serial
    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
        state = in.readBoolean();
    }

    /**
     * Writes the object to a serialized stream.
     * @param out The output stream to write to.
     *            @throws IOException If there is an error writing to the stream.
     */
    @Serial
    private void writeObject(ObjectOutputStream out) throws IOException {
        out.writeBoolean(state);
    }
}
