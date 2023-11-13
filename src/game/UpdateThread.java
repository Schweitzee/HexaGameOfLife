package game;

public class UpdateThread extends Thread{


        private final HexMatrix table;

        private int sleeptime;// = 200;

        private Boolean go = true;

        public UpdateThread(HexMatrix matrix, int sleeptime){
            table = matrix;
            this.sleeptime = sleeptime;
        }

        public void setSleeptime(int time){sleeptime = time;}

        public void pause(){
            go = false;
        }


        @Override
        public void run() {
            while(go){
                try {
                    table.refresh();
                    sleep(sleeptime);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
        }
    }

