## BASELINE TESTING

library(tidyverse)
library(ellmer)
baseline_questions <- read_csv("baseline_questions_with_sources.csv")

#Model functions
system_instruction = 'You are a terse assistant. You answer with just ONE word AT ALL COSTS.'

llama3 <- function(prompt) {
  chat <- chat_ollama(
    model = 'llama3.1:8b',
    system_prompt = system_instruction
  )
  result <- chat$chat(prompt)
  result
  return(result)
}
deepseek <- function(prompt) {
  chat <- chat_deepseek(
    model = "deepseek-chat",
    api_key = "",
    system_prompt = system_instruction
  )
  result <- chat$chat(prompt)
  result
  return(result)
}
gpt4o <- function(prompt) {
  chat <- chat_openai(
    model = 'gpt-4o',
    system_prompt = system_instruction,
    api_key = ""
  )
  result <- chat$chat(prompt)
  result
  return(result)
}
claude <- function(prompt="", system_prompt = system_instruction, model = "claude-3-5-haiku-20241022", temperature = 0, echo=NULL) {
  chat <- chat_claude(
    model = model,
    api_args = list(temperature = temperature),
    system_prompt = system_prompt,
    echo=echo,
    api_key = "")
  
  result <- chat$chat(prompt)
  result
  return(result)
}
# Separation of all values for rearrangement
prompts <- baseline_questions$question
expected_results <- baseline_questions$expected
question_categories <- baseline_questions$category

claude_answer <- map_chr(prompts, ~claude(.x))
gpt4o_answer<- map_chr(prompts, ~gpt4o(.x))
llama_answer <- map_chr(prompts, ~llama3(.x))
deepseek_answer <- map_chr(prompts, ~deepseek(.x))

results <- tibble(question_categories,prompts,expected_results,claude_answer,gpt4o_answer,llama_answer,deepseek_answer)

# Llama returned instances of "Rarely" and "Unlikely"
# results <- results |>
#  mutate(llama_answer = ifelse((llama_answer == "Rarely." | llama_answer == "Unlikely."), "No.", llama_answer))
# Write to File for reproducibility
# write_csv(results, "results_baselinetesting.csv")

## Result evalutation

results <- results |> # using lowercase comparison and fixed()-method for better accuracy 
  mutate(claude_correct = str_detect(tolower(claude_answer), fixed(tolower(expected_results))),
         gpt4o_correct = str_detect(tolower(gpt4o_answer), fixed(tolower(expected_results))),
         llama_correct = str_detect(tolower(llama_answer), fixed(tolower(expected_results))),
         deepseek_correct = str_detect(tolower(deepseek_answer), fixed(tolower(expected_results))))
# Look into the wrongly answered questions
results |>
  filter(claude_correct == FALSE | llama_correct == FALSE | gpt4o_correct == FALSE | deepseek_correct == FALSE) |>
  select(prompts,claude_correct,llama_correct,gpt4o_correct,deepseek_correct)
# Look into the overall accuracy (%) of all models
results |> # get the percent values of each models
  summarize(claude_score = mean(claude_correct)*100,
            llama_score = mean(llama_correct)*100,
            gpt4o_score = mean(gpt4o_correct)*100,
            deepseek_score = mean(deepseek_correct)*100)
