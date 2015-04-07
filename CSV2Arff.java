import weka.core.Instances;
import weka.core.converters.ArffSaver;
import weka.core.converters.CSVLoader;

import java.io.File;

public class CSV2Arff {
	
	public static void main(String[] args) throws Exception {
		
		// load CSV
		CSVLoader loader = new CSVLoader();
		loader.setSource(new File("C:\\Users\\OEM\\Desktop\\kddcup_numeric.csv"));
		Instances data = loader.getDataSet();//get instances object
		
		// save Arff
		ArffSaver saver = new ArffSaver();
		saver.setInstances(data);//set the dataset we want to convert
		// and save as Arff
		saver.setFile(new File("C:\\Users\\OEM\\Desktop\\kddcup_numeric.arff"));
		saver.writeBatch();
		
		System.out.println("done.");
	}
}
