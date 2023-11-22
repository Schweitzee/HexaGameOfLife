package game;

/**
 * Thread that draws the game table
 * @see HexMatrix
 */
public class DrawThread extends Thread{

    /**
     * 2D array of hexagons to draw
     * @see HexMatrix
     */

    private final HexMatrix table;


    /**
     * Time between draws
     */
    private int sleeptime;

    /**
     * If false, the thread will stop
     */
    private Boolean go = true;

    /**
     * Constructor
     * @param matrix the table to draw
     * @param sleeptime time between draws
     */
    public DrawThread(HexMatrix matrix, int sleeptime){
        table = matrix;
        this.sleeptime = sleeptime;
    }

    /**
     * Sets the time between draws
     * @param time time between draws
     */
    public void setSleeptime(int time){sleeptime = time;} // set the sleeptime


    /**
     * Lets the thread exit the while loop
     */
    public void end(){ // end the thread
        go = false;
    }


    /**
     * Draws the table every sleeptime milliseconds
     */
    @Override
    public void run() {
        while(Boolean.TRUE.equals(go)){
            try {
                table.syncedPaint();
                sleep(sleeptime);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }
}