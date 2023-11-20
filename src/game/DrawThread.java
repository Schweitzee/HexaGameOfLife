package game;

public class DrawThread extends Thread{

    private final HexMatrix table;

    private int sleeptime;

    private Boolean go = true;

    public DrawThread(HexMatrix matrix, int sleeptime){
        table = matrix;
        this.sleeptime = sleeptime;
    }


    public void setSleeptime(int time){sleeptime = time;}



    public void end(){
        go = false;
    }


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