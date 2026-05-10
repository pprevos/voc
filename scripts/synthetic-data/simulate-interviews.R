# Generate simulated interviews with water utility customer service employees
library(gemini.R)

# Setup
api_key <- readLines("scripts/synthetic-data/gemini_api.txt")
setAPI(api_key)

# A variety of personas
personas <- c(
  "A friendly, highly experienced agent from a rural water utility.
   She uses mainly annecdotes about her experiences, telling stories from the customer's perspective, with using any names.",
  "A fast-talking young professional from a major city using corporate jargon. 
   He thinks customers complain to much and that they should be happy with their service.",
  "A former field technician who moved into the call center after an injury. 
   Uses many technical terms (mains, meters, backflow) and speaks with a dry, matter-of-fact tone.
   Sympathetic but tired and frustrated agent working the late shift",
  "A young professional with not much experience in the role.  
   She is very precise, uses full sentences, and is careful to mention \"compliance,\" \"frameworks,\" and \"regulatory requirements.\""
)

# List of questions for each interview
questions <- c(
  "To start off, could you just tell us a bit about yourself and what a typical day looks like for you at the utility?",
  "How do you usually handle a call where a customer reports 'cloudy' or 'milky' water and is worried about their health?",
  "How do you explain a sudden spike in a water bill to someone who is adamant that their usage habits haven't changed at all?",
  "Tell me about a time you dealt with a truly angry customer and managed to turn the situation around.",
  "Describe a situation where you had to explain a planned service interruption to a business owner who was worried about losing revenue.",
  "Describe a time you handled a billing call where it became clear the customer simply couldn't afford to pay.",
  "Can you talk about a time you had to coordinate with the field crews or the technical team to solve a customer's issue?",
  "How do you respond to older or less tech-savvy customers who are frustrated by the push toward digital billing and 'smart' meters?",
  "Have you ever had a situation where a customer asked you to 'bend the rules' on a fee or a restriction? How did you handle that?",
  "This job can be tough, especially during peak periods or emergencies. How do you keep your cool when the phones are ringing off the hook?",
  "That’s all the specific scenarios I had. Are there any final comments or thoughts you'd like to share about your role?"
)

simulate_interview <- function(persona, questions) {
  transcript_text <- character()
  for (q in questions) {
    prompt <- paste0(
      "Act as a water customer service agent working for an imaginary water utility. Persona: ", persona, ".\n",
      "Answer the following interview question as a spoken response.\n",
      "CRITICAL: Format as a raw verbatim transcript. Include natural speech patterns like ",
      "'um', 'ah', 'like', 'sort of', and occasional self-corrections or false starts. ",
      "Do not use bullet points or Markdown formatting. Use plain, conversational text.",
      "IMPORTANT: Do not use any company names or names of people, real or imagined\n\n",
      "Interviewer: ", q, "\n",
      "Agent:")
    response <- gemini(prompt, timeout = 300) 
    entry <- paste0("INTERVIEWER: ", q, "\n\nAGENT: ", gsub("\n\n", "\n", response), "\n\n")
    transcript_text <- paste0(transcript_text, entry)
    Sys.sleep(1.5) # Pace the API
  }
  return(transcript_text)
}

# Generation Loop
for (i in 1:length(personas)) {
  message("Interview ", i, ": \n", personas[i])
  transcript <- simulate_interview(personas[i], questions)
  filename <- sprintf("data/synthetic-interviews/raw_transcript_%02d.txt", i)
  writeLines(transcript, filename)
  message("Saved ", filename, "\n")
}
