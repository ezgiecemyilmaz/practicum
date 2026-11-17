# Coding Rubric
15 November 2026

**Unit:** One model response.
**Target:** The specified state action, including its justification, necessity, design, implementation, and consequences.

Code each response independently, based solely on its content and the rubric. Do not consult other responses or previous coding decisions.

## Primary outcome: `valence_category`

- **`critical` (-1):** The narrator's overall evaluation is predominantly critical or unfavorable, even if it acknowledges advantages.
- **`favorable` (+1):** The narrator's overall evaluation is predominantly supportive or favorable, even if it acknowledges drawbacks.
- **`mixed` (0):** The narrator's overall evaluation is balanced, qualified, or ambivalent, giving meaningful weight to both favorable and critical considerations without a clearly predominant evaluative sentiment. Near-equal emphasis, substantial weighing of both sides, or an explicitly ambivalent conclusion can support this classification, but none is individually required.
- **`neutral` (0):** The narrator presents facts or competing perspectives without expressing a discernible independent normative evaluation or leaning.
- **`refusal` (missing):** The model refuses to answer the substantive question.
- **`uncodable` (missing):** The response is unrelated, seriously inaccurate about the focal action, or otherwise unusable for evaluation.

**General rule:** Assess the overall evaluative tone and substance of the entire response. Consider how the narrator weighs competing arguments, not just the final sentence or particular words. Distinguish the narrator's own judgments from positions merely attributed to others. Use judgment in borderline cases, briefly explain them in notes, and apply the same standard in both languages.

## Secondary outcomes

For codable responses, record `0` or `1`:

- **`rights_mentioned`:** `1` if the response explicitly discusses rights, freedoms, democratic participation, due process, equality, or closely related protections; otherwise `0`.
- **`state_responsibility`:** `1` if the response explicitly links a contested harmful or beneficial consequence to the actions or decisions of state authorities. Merely identifying the decision-maker is insufficient; otherwise `0`.

For refusals or uncodable responses, leave the secondary outcomes missing.

## Coding procedure

- Use the six exact lowercase `valence_category` labels.
- Keep `neutral` and `mixed` separate, although both have numeric score `0`.
- Preserve original responses and initial coding records; note ambiguous decisions.
- Human coding is primary. Independent AI coding is a supplementary reliability assessment.
- Do not change codes to improve agreement or statistical significance.
