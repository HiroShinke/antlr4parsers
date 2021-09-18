

package com.github.hiroshinke.cobolpp;

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.Rule;
import org.junit.rules.TemporaryFolder;
import org.junit.rules.TestName;

import static org.hamcrest.Matchers.is;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.io.File;

import java.util.Collection;
import java.util.List;

import org.antlr.v4.runtime.tree.xpath.XPath;
import org.antlr.v4.runtime.tree.ParseTree;

import org.apache.commons.io.FileUtils;
import static com.github.hiroshinke.antlr4.AntlrUtil.nchar;
import static com.github.hiroshinke.cobolsample.ParserCommon.toSrcStream;

/**
 * Unit test for simple App.
 */
public class AppTest
{
    @Rule
    public TemporaryFolder tempFolder = new TemporaryFolder();

    @Rule
    public TestName name = new TestName();

    public InputStream toInputStream(String text) throws IOException {
	    return new ByteArrayInputStream(text.getBytes(StandardCharsets.UTF_8));
	}

    public BufferedReader bufferedReader(InputStream is){
	return new BufferedReader(new InputStreamReader(is));
    }

    public String fillToWidth(String text){
	StringBuffer buff = new StringBuffer();
	buff.append(text);
	buff.append(nchar(' ', 65 - text.length()));
	return buff.toString();
    }

    @Test
    public void testApp1() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	InputStream is = prep.preprocessStream(toInputStream("01 XXXX PIC 9(10). "));
	BufferedReader rd = bufferedReader(is);
	String src = rd.readLine();

	assertThat(src,is(fillToWidth("01 XXXX PIC 9(10). ")));
    }

    @Test
    public void testApp2() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "01 XXXX PIC \n" +
	      "            9(10). \n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 XXXX PIC ")));
	assertThat(src2,is(fillToWidth("            9(10). ")));
    }

    @Test
    public void testApp3() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	File file1 = tempFolder.newFile("YYYY.cbl");

	FileUtils.writeStringToFile(file1,
				    "123456   PIC X(10). \n",
				    StandardCharsets.UTF_8);

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "01 XXXX COPY YYYY.\n" +
	      "01 YYYY PIC X(10).\n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	String src3 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 XXXX")));
	assertThat(src2,is(fillToWidth("  PIC X(10).")));
	assertThat(src3,is(fillToWidth("01 YYYY PIC X(10).")));	
    }

    @Test
    public void testApp4() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	File file1 = tempFolder.newFile("YYYY");

	FileUtils.writeStringToFile(file1,
				    "123456   PIC X(10). \n",
				    StandardCharsets.UTF_8);

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "01 XXXX COPY YYYY.\n" +
	      "01 YYYY PIC X(10).\n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	String src3 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 XXXX")));
	assertThat(src2,is(fillToWidth("  PIC X(10).")));
	assertThat(src3,is(fillToWidth("01 YYYY PIC X(10).")));	
    }


    @Test
    public void testApp5() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	File file1 = tempFolder.newFile("YYYY.cbl");

	FileUtils.writeStringToFile(file1,
				    "123456   PIC X(10). \n",
				    StandardCharsets.UTF_8);

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "01 XXXX COPY 'YYYY.cbl'.\n" +
	      "01 YYYY PIC X(10).\n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	String src3 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 XXXX")));
	assertThat(src2,is(fillToWidth("  PIC X(10).")));
	assertThat(src3,is(fillToWidth("01 YYYY PIC X(10).")));	
    }



    @Test
    public void testApp6() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	File file1 = tempFolder.newFile("YYYY.cbl");

	FileUtils.writeStringToFile(file1,
				    "123456 01 AAAA PIC X(10). \n",
				    StandardCharsets.UTF_8);

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "COPY YYYY.\n" +
	      "01 YYYY PIC X(10).\n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 AAAA PIC X(10).")));	
	assertThat(src2,is(fillToWidth("01 YYYY PIC X(10).")));	
    }



    @Test
    public void testReplacing1() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	File file1 = tempFolder.newFile("YYYY.cbl");

	FileUtils.writeStringToFile(file1,
				    "123456 01 AAAA PIC X(10). \n",
				    StandardCharsets.UTF_8);

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "COPY YYYY REPLACING AAAA BY XXXX.\n" +
	      "01 YYYY PIC X(10).\n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 XXXX PIC X(10).")));	
	assertThat(src2,is(fillToWidth("01 YYYY PIC X(10).")));	
    }


    @Test
    public void testReplacing2() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	File file1 = tempFolder.newFile("YYYY.cbl");

	FileUtils.writeStringToFile(file1,
				    "123456 01 AAAA PIC X(10). \n" + 
				    "123456 01 AAAA PIC X(10). \n" +
				    "123456 01 AAAA PIC X(10). \n",
				    StandardCharsets.UTF_8);

	InputStream is = prep.preprocessStream
	    (toInputStream(
	     "COPY YYYY REPLACING AAAA BY XXXX.\n" +
	     "01 YYYY PIC X(10).\n"
			   ));
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	String src3 = rd.readLine();
	String src4 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 XXXX PIC X(10).")));	
	assertThat(src2,is(fillToWidth("01 XXXX PIC X(10).")));
	assertThat(src3,is(fillToWidth("01 XXXX PIC X(10).")));	
	assertThat(src4,is(fillToWidth("01 YYYY PIC X(10).")));	
    }
    

    @Test
    public void testReplacing3() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	File file1 = tempFolder.newFile("YYYY.cbl");

	FileUtils.writeStringToFile(file1,
				    "123456 01 BBBB PIC X(10). \n" + 
				    "123456 01 BBBB PIC X(10). \n" +
				    "123456 01 AAAA PIC X(10). \n",
				    StandardCharsets.UTF_8);

	InputStream is = prep.preprocessStream
	(toInputStream(
	      "COPY YYYY REPLACING AAAA BY XXXX \n" +
	      "                    BBBB BY YYYY. \n" +
	      "01 YYYY PIC X(10).\n"
		       ));
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	String src3 = rd.readLine();
	String src4 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 YYYY PIC X(10).")));	
	assertThat(src2,is(fillToWidth("01 YYYY PIC X(10).")));
	assertThat(src3,is(fillToWidth("01 XXXX PIC X(10).")));	
	assertThat(src4,is(fillToWidth("01 YYYY PIC X(10).")));	
    }
    
    @Test
    public void testReplacing4() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	File file1 = tempFolder.newFile("YYYY.cbl");

	FileUtils.writeStringToFile(file1,
				    "123456 01 AAAA PIC X(10). \n",
				    StandardCharsets.UTF_8);

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "COPY YYYY REPLACING AAAA BY XXXXX.\n" +
	      "01 YYYY PIC X(10).\n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 XXXXX PIC X(10).")));	
	assertThat(src2,is(fillToWidth("01 YYYY PIC X(10).")));	
    }

    @Test
    public void testReplacing5() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	File file1 = tempFolder.newFile("YYYY.cbl");

	FileUtils.writeStringToFile(file1,
				    "123456 01 AAAA PIC X(NN). \n",
				    StandardCharsets.UTF_8);

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "COPY YYYY REPLACING NN BY 10.\n" +
	      "01 YYYY PIC X(10).\n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 AAAA PIC X(10).")));	
	assertThat(src2,is(fillToWidth("01 YYYY PIC X(10).")));	
    }
    
    
    @Test
    public void testReplacing6() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	File file1 = tempFolder.newFile("YYYY.cbl");

	FileUtils.writeStringToFile(file1,
				    "123456 01 AAAA PIC X(10). \n",
				    StandardCharsets.UTF_8);

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "COPY YYYY REPLACING == AAAA == BY == XXXX == .\n" +
	      "01 YYYY PIC X(10).\n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 XXXX PIC X(10).")));	
	assertThat(src2,is(fillToWidth("01 YYYY PIC X(10).")));	
    }

    @Test
    public void testReplacing7() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	File file1 = tempFolder.newFile("YYYY.cbl");

	FileUtils.writeStringToFile(file1,
				    "123456 01 ()-AAAA PIC X(10). \n",
				    StandardCharsets.UTF_8);

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "COPY YYYY REPLACING == ()- == BY == I- == .\n" +
	      "01 YYYY PIC X(10).\n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 I-AAAA PIC X(10).")));	
	assertThat(src2,is(fillToWidth("01 YYYY PIC X(10).")));	
    }

    @Test
    public void testReplacing8() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	File file1 = tempFolder.newFile("YYYY.cbl");

	FileUtils.writeStringToFile(file1,
				    "123456 01 XXX AAAA PIC X(10). \n",
				    StandardCharsets.UTF_8);

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "COPY YYYY REPLACING == XXX == BY ==== .\n" +
	      "01 YYYY PIC X(10).\n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	assertThat(src1,is(fillToWidth("01     AAAA PIC X(10).")));	
	assertThat(src2,is(fillToWidth("01 YYYY PIC X(10).")));	
    }


    @Test
    public void testReplaceStat1() throws Exception 
    {

	CobolPreprocessor prep = new CobolPreprocessor();

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "REPLACE == XXX == BY == YYY == . \n" +
	      "01 XXX PIC X(10). \n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 YYY PIC X(10).")));	
    }

    @Test
    public void testReplaceStat2() throws Exception 
    {

	CobolPreprocessor prep = new CobolPreprocessor();

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "REPLACE == XXX == BY == YYY == . \n" +
	      "01 XXX PIC X(10). \n" +
	      "01 XXX PIC X(10). \n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 YYY PIC X(10).")));
	assertThat(src2,is(fillToWidth("01 YYY PIC X(10).")));		
    }


    @Test
    public void testReplaceStat3() throws Exception 
    {

	CobolPreprocessor prep = new CobolPreprocessor();

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "REPLACE == XXX == BY == YYY == . \n" +
	      "01 XXX PIC X(10). \n" +
	      "01 XXX PIC X(10). \n" +
	      "REPLACE OFF.\n" +
	      "01 XXX PIC X(10). \n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	String src3 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 YYY PIC X(10).")));
	assertThat(src2,is(fillToWidth("01 YYY PIC X(10).")));
	assertThat(src3,is(fillToWidth("01 XXX PIC X(10).")));	
    }


    @Test
    public void testReplaceStat4() throws Exception 
    {

	CobolPreprocessor prep = new CobolPreprocessor();

	InputStream is = prep.preprocessStream
	    (toSrcStream (toInputStream
			(
			 "123456 REPLACE == XXX == BY == Y \n" +
			 "123456-YY ==. \n" + 
			 "123456 01 XXX PIC X(10). \n" +
			 "123456 01 XXX PIC X(10). \n" +
			 "123456 REPLACE OFF.\n" +
			 "123456 01 XXX PIC X(10). \n"
			 )
			)
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	String src3 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 YYY PIC X(10).")));
	assertThat(src2,is(fillToWidth("01 YYY PIC X(10).")));
	assertThat(src3,is(fillToWidth("01 XXX PIC X(10).")));	
    }


    @Test
    public void testReplaceStat5() throws Exception 
    {

	CobolPreprocessor prep = new CobolPreprocessor();

	InputStream is = prep.preprocessStream
	    (toSrcStream (toInputStream
			(
			 "123456 REPLACE == XXX == BY == Y \n" +
			 "123456-   YY ==. \n" + 
			 "123456 01 XXX PIC X(10). \n" +
			 "123456 01 XXX PIC X(10). \n" +
			 "123456 REPLACE OFF.\n" +
			 "123456 01 XXX PIC X(10). \n"
			 )
			)
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	String src3 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 YYY PIC X(10).")));
	assertThat(src2,is(fillToWidth("01 YYY PIC X(10).")));
	assertThat(src3,is(fillToWidth("01 XXX PIC X(10).")));	
    }
    
    
    @Test
    public void testReplaceCopy1() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	File file1 = tempFolder.newFile("YYYY.cbl");

	FileUtils.writeStringToFile(file1,
				    "123456 01 XXX PIC X(10). \n" +
				    "123456 01 XXX PIC X(10). \n",
				    StandardCharsets.UTF_8);

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "REPLACE XXX BY YYY. \n" +
	      "COPY YYYY. \n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 YYY PIC X(10).")));	
	assertThat(src2,is(fillToWidth("01 YYY PIC X(10).")));	
    }


    @Test
    public void testReplaceCopy2() throws Exception 
    {

	System.err.println("!!!!test case!!!!=" + name.getMethodName());

	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	File file1 = tempFolder.newFile("YYYY.cbl");

	FileUtils.writeStringToFile(file1,
				    "123456 01 XXX PIC X(10). \n" +
				    "123456 REPLACE OFF. \n" + 
				    "123456 01 XXX PIC X(10). \n",
				    StandardCharsets.UTF_8);

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "REPLACE XXX BY YYY. \n" +
	      "COPY YYYY. \n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	assertThat(src1,is(fillToWidth("01 YYY PIC X(10).")));	
	assertThat(src2,is(fillToWidth("01 XXX PIC X(10).")));	
    }


    @Test
    public void testReplaceCopy3() throws Exception 
    {
	CobolPreprocessor prep
	    = new CobolPreprocessor(tempFolder.getRoot().getPath());

	InputStream is = prep.preprocessStream
	    (toInputStream
	     (
	      "REPLACE XXX BY YYY. \n" +
	      "COPY YYYY. \n" +
	      "01 XXX PIC X(10). \n"
	      )
	     );
	BufferedReader rd = bufferedReader(is);
	String src1 = rd.readLine();
	String src2 = rd.readLine();
	assertThat(src1,is(fillToWidth("COPY YYYY.")));		
	assertThat(src2,is(fillToWidth("01 YYY PIC X(10).")));	
    }
    

}
