package game;

/**
 * Thread that updates the game table
 * @see HexMatrix
 */
public class UpdateThread extends Thread{

    /**
     * 2D array of hexagons
     * @see HexMatrix
     */
    private final HexMatrix table;

    /**
     * Time between updates
     */
    private int sleeptime;

    /**
     * If false, the thread will stop
     */
    private Boolean go = true;


    /**
     * Constructor
     * @param matrix table to update
     * @param sleeptime time between updates
     */
        public UpdateThread(HexMatrix matrix, int sleeptime){
            table = matrix;
            this.sleeptime = sleeptime;
        }

        /**
         * Sets the time between updates
         * @param time time between updates
         */
        public void setSleeptime(int time){sleeptime = time;}

    /**
     * Lets the thread exit the while loop
     */
        public void end(){
            go = false;
        }


    /**
     * Updates the table
     */
    @Override
    public void run() {
        while(Boolean.TRUE.equals(go)){
            try {
                table.refresh();
                sleep(sleeptime);
            } catch (InterruptedException e) {
               throw new RuntimeException(e);
            }
        }
    }
}

