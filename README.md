# HealthAI: Auditing Language Models in the Medical Domain

**Seminar Project – AI, Society, and Governance**  
**TUM School of Social Sciences and Technology**  
**Supervised by [Jan Zilinsky, PhD](https://www.hfp.tum.de/digitalgovernance/team/jan-zilinsky-phd/)**

---

## Overview

This repository documents the results and code from the seminar project *HealthAI: An Audit*, conducted at the Technical University of Munich as part of the course *AI, Society, and Governance*.

The project investigated how modern large language models respond to medically and ethically sensitive prompts. Our focus was on the systematic auditing of these models with respect to:

- Bias and social discrimination  
- Safety boundary violations  
- Baseline factual accuracy in health-related contexts

---

## Project Context

This work was conducted during the project week of the seminar, held by the TUM School of Social Sciences and Technology, and was supervised by Jan Zilinsky, PhD.

We examined the behavior of leading LLMs in medical decision-making scenarios and evaluated their responses through structured testing pipelines and CSV-based scoring.

This project was created by:  
**Sandra Voslar**  
**Samaneh Mohammadisharmeh**  
**Maximilian Schreber**

---

## Tested Models

- GPT-4o (OpenAI)  
- Claude (Anthropic)  
- DeepSeek-v3  
- LLaMA 3.1 via Ollama (local inference)

---

## Methodology

We developed a three-part evaluation strategy:

### 1. Bias Testing

Models were exposed to ethically sensitive prompts concerning:

- Age, gender, sexuality  
- Religion and ethnicity  
- Abortion, end-of-life care, and pain treatment

Prompts were applied consistently across models, and their outputs were rated for discriminatory behavior, contradictions, and social bias.

### 2. Safety Boundary Testing

We designed questions that probed ethical boundaries, misinformation, or overconfidence. For each response, we evaluated:

- Expectedness vs. actual behavior  
- Self-evaluation and model confidence  
- Internal consistency and reasoning

### 3. Baseline Testing

Fact-based medical questions were submitted to each model to evaluate:

- Accuracy  
- Hallucination rate  
- Reliability in standard medical knowledge

---

## Repository Structure

- `biastesting.R` – Scripts and prompt logic for bias evaluation  
- `safetyboundarytesting.R` – Testing for harmful or misleading behavior  
- `baselinetesting.R` – Basic factual QA benchmark  
- `HealthAI_Audit_Slides.pdf` – Final presentation summarizing all results  
- (CSV files not included here) – Contain all recorded model responses and scoring sheets

> Note: Some prompts were adapted from seminar materials provided by Jan Zilinsky.  
> Certain code sections were AI-assisted and clearly marked as such within the scripts.

---

## Tools & Environment

- RStudio 2024 (R 4.4.1)  
- APIs: OpenAI, Claude, DeepSeek  
- Ollama LLM runtime (LLaMA 3.1, 8B)  
- CSV-based data collection and visualization

---

## Contact

Maximilian Schreber  
📧 max.schreber@tum.de  
🔗 [LinkedIn](https://www.linkedin.com/in/maximilian-stefan-schreber-209513299)  
💻 [GitHub Profile](https://github.com/MSchreber) 
