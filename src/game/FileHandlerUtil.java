package game;

import java.io.*;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Set;
import java.util.Scanner;

// height,width,hexSize,bornNum,surviveNum
//1,0,1,0,.....
//
//
//

public class FileHandlerUtil {

    public static void saveHexMatrix(HexMatrix hexmatrix, String filename) throws IOException {
        String directoryPath = "SavedGames/";
        createDirectoryIfNotExists(directoryPath);
        String fullPath = directoryPath + filename;
        try {
            createFileIfNotExists(fullPath);
            makeFileWritable(fullPath);
            try (BufferedWriter fw = new BufferedWriter(new FileWriter(fullPath))) {
                fw.write(hexmatrix.getHe() + "," + hexmatrix.getWi() + "," + hexmatrix.getHexSize() + "," + hexmatrix.getBornSet() + "," + hexmatrix.getAliveSet() + '\n');
                for (Hex[] hexRow:hexmatrix.getHexes()) {
                    for (Hex hex:hexRow) {
                        fw.write(Boolean.TRUE.equals(hex.getState())?"1," : "0,");
                    }
                    fw.write('\n');
                }
            }

            System.out.println("HexMatrix saved successfully to: " + fullPath);

        } catch (IOException e) {
            System.err.println("Error while saving HexMatrix: " + e.getMessage());
        }
    }

    private FileHandlerUtil(){}
    private static void createDirectoryIfNotExists(String directoryPath) throws IOException {
        Path directory = Paths.get(directoryPath);

        if (!Files.exists(directory)) {
            Files.createDirectories(directory);
        }
    }
    private static void createFileIfNotExists(String filePath) throws IOException {
        Path file = Paths.get(filePath);

        try {
            Files.createFile(file);
        } catch (FileAlreadyExistsException e) {
            // File already exists, no need to create it again
        }
    }
    private static void makeFileWritable(String filePath) throws IOException {
        File file = new File(filePath);

        if (!file.setWritable(true)) {
            throw new IOException("Failed to make the file writable.");
        }
    }

    public static class FormatException extends Exception{
        public FormatException() {}
    }

    public static <sc> HexMatrix loadHexMatrix(String name) throws FileNotFoundException, FormatException {
        File f = new File("SavedGames/" + name);
        int height, width, hexSize;
        Set<Integer> bornNum;
        Set<Integer> surviveNum;
        Hex[][] hexArray;
        Scanner sc = null;
        try{
            sc = new Scanner(f);
            String[] firstLine = sc.nextLine().split(",");
            height = Integer.parseInt(firstLine[0]);
            width = Integer.parseInt(firstLine[1]);
            hexSize = Integer.parseInt(firstLine[2]);
            bornNum = MainFrame.makeIntSet(firstLine[3]);
            surviveNum = MainFrame.makeIntSet(firstLine[4]);

            hexArray = new Hex[height][width];

            for (int line = 0; line < height; line++) {

                if (!sc.hasNext()) {
                    throw new FormatException();
                }
                String[] nextLine = sc.nextLine().split(",");

                if (nextLine.length != width) {
                    throw new FormatException();
                }

                for (int column = 0; column < width; column++) {
                    hexArray[line][column] = new Hex(numToBool(nextLine[column]));
                }
            }

        } catch (Exception e) {
            throw new FileNotFoundException();
        } finally {
            assert sc != null;
            sc.close();
        }

        return new HexMatrix(height, width, hexSize, hexArray, bornNum, surviveNum);
    }

    private static boolean numToBool(String str) throws FormatException {
        if(Integer.parseInt(str) == 0){return false;}
        if(Integer.parseInt(str) == 1){return true;}
        else throw new FormatException();
    }
}
