package game;

import java.io.File;
import java.io.FileNotFoundException;
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

   public void saveHexMatrix(HexMatrix hexmatrix, String filename){
       File saving = new File("SavedGames/" + filename + ".txt");
       try{
           if(saving.createNewFile()){
                System.out.println("Game saved successfully");
           }
       } catch (IOException e) {
           System.out.println("Problem occurred with game saving, probably restricted access in the saving directory");
       }

       /*SAVE TO BE IMPLEMENTED*/

   }



    private static boolean numToBool(String str) throws FormatException {
        if(Integer.parseInt(str) == 0){return false;}
        if(Integer.parseInt(str) == 1){return true;}
        else throw new FormatException();
    }
}
