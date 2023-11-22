package game;

import java.io.*;
import java.nio.file.*;


// height,width,hexSize,bornNum,surviveNum
//1,0,1,0,.....
//
//
//

/**
 * Utility class for saving and loading HexMatrix objects to and from files.
 */
public class FileHandlerUtil {


    /**
     * Saves a HexMatrix object to a file.
     *
     * @param hexmatrix The HexMatrix object to save.
     * @param filename  The name of the file to save to.
     * @throws IOException If there is an error saving the file.
     */
    public static void saveHexMatrix(HexMatrix hexmatrix, String filename) throws IOException {
        // Specify the directory where the files should be saved
        String saveDirectory = "SavedGames/";

        // Create the full path including the directory
        Path filePath = FileSystems.getDefault().getPath(saveDirectory, filename);

        // Check if the file already exists
        if (Files.exists(filePath)) {
            throw new FileAlreadyExistsException("File already exists: " + filename);
        }

        // Create the parent directories if they don't exist
        if (!Files.exists(filePath.getParent())) {
            Files.createDirectories(filePath.getParent());
        }

        try (FileOutputStream fileOutputStream = new FileOutputStream(filePath.toFile());
             ObjectOutputStream objectOutputStream = new ObjectOutputStream(fileOutputStream)) {

            objectOutputStream.writeObject(hexmatrix);
            System.out.println("HexMatrix saved successfully to " + filePath);
        } catch (IOException e) {
            System.err.println("Error saving HexMatrix to " + filePath);
            throw e;
        }
    }

    /**
     * Private constructor to prevent instantiation.
     */
    private FileHandlerUtil(){} // Prevent instantiation


    /**
     * Loads a HexMatrix object from a file.
     *
     * @param filename The name of the file to load from.
     * @return The loaded HexMatrix object.
     * @throws IOException            If there is an error loading the file.
     * @throws ClassNotFoundException If the file does not contain a valid HexMatrix object.
     */
    public static HexMatrix loadHexMatrix(String filename) throws IOException, ClassNotFoundException {
        // Specify the directory where the files are saved
        String saveDirectory = "SavedGames/";

        // Create the full path including the directory
        Path filePath = FileSystems.getDefault().getPath(saveDirectory, filename);

        try (FileInputStream fileInputStream = new FileInputStream(filePath.toFile());
             ObjectInputStream objectInputStream = new ObjectInputStream(fileInputStream)) {

            Object loadedObject = objectInputStream.readObject();

            if (loadedObject instanceof HexMatrix) {
                System.out.println("HexMatrix loaded successfully from " + filePath);
                return (HexMatrix) loadedObject;
            } else {
                throw new IOException("File does not contain a valid HexMatrix");
            }
        } catch (IOException | ClassNotFoundException e) {
            System.err.println("Error loading HexMatrix from " + filePath);
            throw e;
        }
    }
}


