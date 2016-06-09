read_yaml <- function(yaml_file = '../credentials.yaml'){
  require(yaml)
  yaml.load_file(yaml_file)
  
}