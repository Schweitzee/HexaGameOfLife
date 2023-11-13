package game;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Set;
import java.util.Scanner;

// height,width,hexSize,bornNum,surviveNum
//1,0,1,0,.....
//
//
//
//
//
//
//

public class FileHandlerUtil {

   public static void saveHexMatrix(HexMatrix hexmatrix, String filename) throws IOException {
       File saving = new File("SavedGames/" + filename + ".txt");
       try{
           saving.createNewFile();
           saving.setWritable(true);
           FileWriter fw = new FileWriter(saving);
           fw.write(hexmatrix.getHe() + "," + hexmatrix.getWi() + "," + hexmatrix.getHexSize() + "," + hexmatrix.getBornSet() + "," + hexmatrix.getAliveSet());
           for (Hex[] hexRow:hexmatrix.getHexes()) {
               for (Hex hex:hexRow) {
                   fw.write(hex.getState()?"1," : "0,");
               }
               fw.write('\n');
           }
       }
       catch (IOException e) {
           System.out.println("Problem occurred with game saving, probably restricted access in the saving directory");
       }
   }
    private FileHandlerUtil(){}

    public static class FormatException extends Exception{
        public FormatException() {}
    }

    public static HexMatrix loadHexMatrix(String name) throws FileNotFoundException, FormatException {
        File f = new File("SavedGames/" + name);
        Scanner sc = new Scanner(f);

        int height, width,hexSize;
        Set<Integer> bornNum;
        Set<Integer> surviveNum;
        String[] firstLine = sc.nextLine().split(",");
        height = Integer.parseInt(firstLine[0]);
        width = Integer.parseInt(firstLine[1]);
        hexSize = Integer.parseInt(firstLine[2]);
        bornNum = MainFrame.makeIntSet(firstLine[3]);
        surviveNum = MainFrame.makeIntSet(firstLine[4]);

        Hex[][] hexArray = new Hex[height][width];

        for(int line = 0; line < height;line++){

            if (!sc.hasNext()){throw new FormatException();}
            String[] nextLine = sc.nextLine().split(",");

            if (nextLine.length != width){throw new FormatException();}

            for(int column = 0; column < width; column++){
                hexArray[line][column] = new Hex(numToBool(nextLine[column]));
            }
        }
        sc.close();
        return new HexMatrix(height,width,hexSize,hexArray,bornNum,surviveNum);
    }

    private static boolean numToBool(String str) throws FormatException {
        if(Integer.parseInt(str) == 0){return false;}
        if(Integer.parseInt(str) == 1){return true;}
        else throw new FormatException();
    }
}
