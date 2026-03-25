# APA-7th_edition_in_text_citation_conversion
Given id's in a text file with main text and references with id's in another file, return a file with replacing all id's in text file


## What you need
- Install Ocaml: 


## How it works
This program will convert reference list to the APA 7th edition intext citation and replace the in-text id with the formatted citation. 

NOTE: 
  - please sort the order of the reference before the application. 
  - same as the in-text citations.
  - FOLLOW THE EXACT FORMAT


Files: 
    - ref.txt   : file containing references
    - text.txt  : file of the main text 

    - out_put   : output file


For example: 

      <<<  ref.text  >>>

214: Persad, L. S., Binder-Markey, B. I., Shin, A. Y., Lieber, R. L., & Kaufman, K. R. (2023). American Society of Biomechanics Journal of Biomechanics Award 2022: Computer models do not accurately predict human muscle passive muscle force and fiber length: Evaluating subject-specific modeling impact on musculoskeletal model predictions. Journal of Biomechanics, 159, Article 111798. https://doi.org/10.1016/j.jbiomech.2023.111798
	https://search.library.uvic.ca/permalink/01VIC_INST/1d1fbv0/cdi_proquest_journals_2869637399

215: Valente, G., Pitto, L., Testi, D., Seth, A., Delp, S. L., Stagni, R., Viceconti, M., & Taddei, F. (2014). Are Subject-Specific Musculoskeletal Models Robust to the Uncertainties in Parameter Identification? PloS One, 9(11), e112625. https://doi.org/10.1371/journal.pone.0112625
	https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0112625&type=printable


      <<<   text.txt   >>>

Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. Some text here (214).
Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. 
Some text here. Some text here. Some text here. Some text here. Some text here (215; 214). Some text here. Some text here. 


      <<<   output.txt   >>>

Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. Some text here (Persad et al., 2023).
Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. 
Some text here. Some text here. Some text here. Some text here. Some text here (Valente et al., 2014; Persad et al., 2023). Some text here. Some text here. 
