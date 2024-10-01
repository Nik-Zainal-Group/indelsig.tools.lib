#include <Rcpp.h>
using namespace std;
using namespace Rcpp;


bool compareSegmentations(std::vector<int> v1, std::vector<int> v2){
                      if(v1[3] > v2[3]){
                        return true;
                      }else if(v1[3] < v2[3]){
                        return false;
                      } else{
                        if(v1[1] > v2[1]){
                          return true;
                        }else if(v1[1] < v2[1]){
                          return false;
                        }else{
                          if(v1[2] < v2[2]){
                            return true;
                          }else if(v1[2] > v2[2]){
                            return false;
                          }else{
                            if(v1[0] < v2[0]){
                              return true;
                            }else if(v1[0] > v2[0]){
                              return false;
                            }else{
                              return false;
                            }
                          }
                        }
                      }
                    }


int ltrs(std::string::iterator str1_start,std::string::iterator str1_end,std::string::iterator str2_start,std::string::iterator str2_end){

                      int increment_unit = std::distance(str1_start,str1_end);
                      int coverage = 0;
                      bool equal_unit=true; // assume all substrings are identica

                      for(auto i = str2_start; i!=str2_end;){
                        for(auto j = str1_start; j!=str1_end; j++,i++){
                          equal_unit = equal_unit & (*i == *j);

                          if(!equal_unit){
                            break;
                          }
                        }

                        if(!equal_unit){ // if is_equal_unit is false, break out.
                          break;
                        }
                        coverage += increment_unit;
                      }
                      return coverage;
                    }


std::vector<int> segmentSingle(std::string &string,std::string &context){
                      int string_size= string.size();
                      std::vector<std::vector<int> > scores(string_size,std::vector<int>(4));

                      int i = 0;
                      for(auto unit_iter = string.begin()+1; unit_iter<=string.end();++unit_iter,++i){
                        scores[i][0]=i+1;
                        scores[i][1]=ltrs(string.begin(),unit_iter,unit_iter,string.end());
                        scores[i][2]=string_size - scores[i][0] - scores[i][1];
                        scores[i][3]=ltrs(string.begin(),unit_iter,context.begin(),context.end());
                      }
                      // in place sort
                      std::sort(scores.begin(),scores.end(),compareSegmentations);
                      return scores[0];
                    }

//' segmentation function
//'
//' @param string A string
//' @param context Context of a string
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame segment(std::vector<std::string> string, std::vector<std::string> context){
                      // init vector
                      int n_strings = string.size();
                      std::vector<std::vector<int> > results(string.size(),std::vector<int>(4));
                      // Apply segment_single via transform
                      std::transform(string.begin(),string.end(),context.begin(),results.begin(),segmentSingle);
                      // Init storage vectors
                      std::vector<std::vector<std::string> > string_results(4,std::vector<std::string>(n_strings));
                      std::vector<std::vector<int> > numeric_results(4,std::vector<int>(n_strings));
                      // Transform results
                      for(int i=0; i < n_strings; i++){
                        // Transpose numeric reults
                        numeric_results[0][i]=results[i][0];
                        numeric_results[1][i]=results[i][1];
                        numeric_results[2][i]=results[i][2];
                        numeric_results[3][i]=results[i][3];
                        // Transform string results
                        string_results[0][i]=string[i].substr(0,numeric_results[0][i]);
                        string_results[1][i]=string[i].substr(numeric_results[0][i],numeric_results[1][i]);
                        string_results[2][i]=string[i].substr(numeric_results[0][i]+numeric_results[1][i],numeric_results[2][i]);
                        string_results[3][i]=context[i].substr(0,numeric_results[3][i]);
                        // Divide internal_reps bases / unit_length , prime3_reps bases / unit_length
                        if(numeric_results[0][i]!=0){
                          numeric_results[1][i]=numeric_results[1][i]/numeric_results[0][i]; // internal_reps // unidivided
                          numeric_results[3][i]=numeric_results[3][i]/numeric_results[0][i]; // prime3_reps // undivided
                        }
                      }

                      // Generate data frame
                      Rcpp::DataFrame df_results = Rcpp::DataFrame::create(
                        _["unit"]=string_results[0],
                                                _["unit_length"]=numeric_results[0],
                                                                                _["internal_rep"]=string_results[1],
                                                                                                                _["internal_reps"]=numeric_results[1],
                                                                                                                                                  _["spacer"]=string_results[2],
                                                                                                                                                                            _["spacer_length"]=numeric_results[2],
                                                                                                                                                                                                              _["prime3_rep"]=string_results[3],
                                                                                                                                                                                                                                            _["prime3_reps"]=numeric_results[3],
                                                                                                                                                                                                                                                                            _["stringsAsFactors"] = false);

                      return df_results;
                    }
