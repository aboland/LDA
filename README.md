# Latent Dirichlet Allocation

Creating functions to simulate documents using the model from [Blei et al., 2002](https://www.cs.princeton.edu/~blei/papers/BleiNgJordan2003.pdf).

Function `GenerateLDA`, which creates a corpus when the user inputs a set of words and paramters $\alpha$ and $\beta$ is working.

**The E-M algorithm or variational inference as described in the paper is not working!**  

## E-M algorithm

### Expectation
First is the expectation step, the function `VariationalExpectationFull` is used for this step. The input arguments are the current values of $\alpha$ and $\beta$, the document term matrix, and a value `epsilon` for checking the convergence of the free variational parameters $\gamma$ and $\phi$.  

`VariationalExpectationFull(alpha, beta, dtm, epsilon = 0.1)`  

The function outputs a value of $\gamma_d$ and $\phi_d$ for **each** document $d$ in the corpus. $\gamma_d$ is always a vector the same length as the number of topics, $\phi_d$ is a matrix with a row for each topic and a column for each word in docoument $d$.

### Maximisation
Next step is the maximisation step, where we maximise the parameters $\alpha$ and $\beta$ given the paramters $\gamma$ and $\phi$. The function `VariationalMaximization` is used to maximise the parameter values, the input arguments are the free variational parameters $\gamma$ and $\phi$, the document term matrix and a paramter `epsilon` to check for convergence of $\alpha$. Changing the algorithm to have a fixed $\alpha$ or a vector where each element is identical might simplify the algorithm significantly.

`VariationalMaximization(variational_para, dtm, epsilon)`

The function outputs values for $\alpha$ and $\beta$.
