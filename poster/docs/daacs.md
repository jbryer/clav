The [Diagnostic Assessment and Achievement of College Skills](https://daacs.net) (DAACS) is a suite of technological and social supports designed to optimize student learning. Students complete assessments in self-regulated learning, writing, mathematics, and reading. The receive immediate feedback in terms of one, two, or three dots (developing, emerging, and mastering, respectively) along with personalized feedback and strategy recommendations.

The goal of this project is to determine if student profiles can be derived from the six main domains: metacognition, strategy use, motivation, writing, mathematics, and reading. If profiles can be estimated this can inform potential future interventions that can vary depending on the students response profile.

The `clav` package was initial designed to test validity of the profiles by repeating split training and validation datasets to look for consistency in the profiles across the two datasets. This has been extended to also allow for using bootstrap samples where cluster profiles are estimated using an "in-bag" sample and then clusters are predicted using the "out-of-bag" sample. This also introduces two new fit statistics:

* The *cluster overlap fit* is a measure of the overlapping distributions of cluster centers for both samples across all variables. Smaller values are desirable since we are looking for separation in cluster centers within each variable. This can also be a useful model diagnostic for determining which variables inform cluster membership.

* The *cluster agreement fit* measures the proportion of observations that are assigned to the same cluster with all other observations. That is, an observation with an overlap measure of 1 means that it always belongs to the same cluster with all of its neighbors.
