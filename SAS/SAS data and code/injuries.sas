data injuries;
input exhaust safety injury tenure serious injuryb latency sex;
datalines;
    3.50    5.50    2.10       5     .00    1.90    4.60     .00
    2.60    3.50    1.00       8     .00    1.10    4.70     .00
    3.40    3.70    2.30       9     .00    2.50    3.30     .00
    4.30    4.40    2.30       8     .00    3.20    5.70     .00
    3.90    4.60    3.00      12    1.00    1.00    3.60     .00
    2.80    3.50    2.00      11     .00    2.60    5.80     .00
    2.20    3.10    1.60       3     .00    2.30    3.00    1.00
    2.50    3.90    2.40       2     .00    2.90    5.50    1.00
    3.50    5.70    3.40       2    1.00    4.50    6.50    1.00
    3.80    4.80    2.90       8    1.00    3.50    4.50     .00
    3.80    2.50    2.10       4     .00    3.10    4.50    1.00
    1.40    2.50    2.50       3     .00    2.30    4.60    1.00
    3.30    4.10    1.80       4     .00    2.30    6.30    1.00
    2.30    5.90    2.50       3     .00    3.50    3.60    1.00
    5.00    4.80    3.70       2    1.00    2.00    4.70    1.00
    4.40    4.90    3.20       8    1.00    2.90    4.40     .00
    5.00    5.00    2.80       5    1.00    1.60    4.80    1.00
    3.60    4.00    2.60       4    1.00     .60    4.30    1.00
    3.10    3.40    1.60       6     .00    2.50    3.40     .00
    1.40    3.10    1.20      10     .00    1.90    2.40     .00
    2.20    3.40    1.70       1     .00    3.00    4.10    1.00
    2.50    4.50    3.00       6    1.00    2.80    4.90     .00
    2.40    2.90    1.50       8     .00    2.20    2.70     .00
    4.40    4.70    1.20       3     .00    1.60    4.60    1.00
    2.00    3.30    1.70      14     .00    3.20    3.30     .00
    4.80    4.00    3.60       5    1.00    2.80    4.60    1.00
    2.80    3.20     .20      13     .00    2.90    4.20     .00
    3.80    4.10    2.00       9     .00    2.00    4.30     .00
    1.00    3.30     .80       6     .00    2.80    4.60     .00
    4.60    2.80    2.40      11     .00    3.10    3.00     .00
    2.60    3.50    1.60       6     .00    2.70    4.50     .00
    3.60    3.70    4.00       6    1.00    3.30    3.80     .00
    3.60    3.90    3.20       2    1.00    3.10    5.50    1.00
    2.70    4.00    2.50       5     .00    2.20    3.80     .00
    1.30    4.50    2.40       2     .00     .70    3.70    1.00
    4.00    6.10    2.60       3    1.00    4.70    6.30    1.00
    1.70    2.30    3.00      18    1.00    3.30    3.00     .00
    3.90    2.30    1.20      11     .00    2.70    3.70     .00
    1.90    4.80    1.50       7     .00    1.70    3.80     .00
    2.20    4.80     .20       5     .00    3.80    5.30    1.00
    3.20    3.50    2.60       3    1.00     .70    3.40    1.00
    1.40    3.10     .50       4     .00    2.90    4.20    1.00
    1.80    3.80    2.10      11     .00    2.00    3.60     .00
    3.40    3.90    1.60       2     .00    1.40    3.20    1.00
    3.60    4.60    4.70       2    1.00    4.30    5.40    1.00
    3.80    3.90    2.50       4     .00    2.40    5.50    1.00
    2.10    3.60    2.00       5     .00    3.70    4.30     .00
    4.00    2.30     .10       7     .00     .70    3.80     .00
    3.30    5.50    1.50       4     .00    3.60    5.80    1.00
    3.90    4.00    2.60       2    1.00    1.50    5.60    1.00
    2.80    3.50    3.00       2    1.00    2.60    4.80    1.00
    3.80    5.20    2.80       2    1.00    3.30    6.30    1.00
    3.10    5.10     .40       1     .00    2.50    4.20    1.00
    5.90    4.00    1.20       5     .00    1.50    5.30     .00
    1.00    2.80    1.50       5     .00    1.90    3.70    1.00
    3.60    4.70    1.50       3     .00    3.60    6.20    1.00
    3.50    2.60    1.30       6     .00    1.80    4.10     .00
    3.50    5.80    1.80       3     .00    3.20    5.30    1.00
    2.40    5.30    2.90       4    1.00    1.70    5.60    1.00
    4.60    4.10    3.00       8    1.00    2.10    4.60     .00
    4.20    5.00     .00       3     .00    3.20    4.20    1.00
    3.40    2.70    1.10      10     .00    2.20    3.60     .00
    4.70    5.00    3.10       3    1.00    2.70    6.50    1.00
    3.80    2.80    2.10       6     .00    2.50    4.50     .00
    1.80    2.80    3.00       5    1.00    2.40    4.00    1.00
    4.80    5.30    2.50       4     .00    4.30    4.80    1.00
    4.80    5.60    1.20       2     .00    2.70    5.70    1.00
    2.60    3.40     .00       4     .00    1.50    3.30    1.00
    3.10    4.70    2.40       6     .00    3.90    5.50     .00
    3.80    5.60    2.60       5    1.00    2.60    5.50     .00
    4.30    4.10    3.10       7    1.00    2.90    4.90     .00
    3.40    4.50    2.70       6    1.00    2.40    2.60     .00
    4.30    6.20    3.40       1    1.00    1.80    6.10    1.00
    3.60    4.30    1.90       3     .00    1.70    4.00    1.00
    4.20    4.00     .90       9     .00    1.90    3.70     .00
    5.80    5.10    2.70       5    1.00     .90    6.70    1.00
    2.20    3.50     .70       4     .00     .90    4.40    1.00
    3.80    2.30     .00      10     .00     .50    2.00     .00
    4.00    3.40     .40       4     .00    2.00    3.10    1.00
    3.10    3.80    3.40       5    1.00    1.90    4.50    1.00
    3.70    3.20    2.00       1     .00    4.10    4.90    1.00
    3.50    2.80    2.40       6     .00    3.40    3.80     .00
    4.40    6.10    3.40       3    1.00    1.90    6.00    1.00
    3.70    5.90    2.10       2     .00    2.70    7.00    1.00
    5.10    2.20     .80      10     .00    1.00    4.40     .00
    4.30    4.80    3.30       4    1.00    3.30    5.20    1.00
    1.90    3.20    2.50       3     .00    1.70    4.00    1.00
    2.40    3.40    1.80      10     .00    1.20    1.90     .00
    4.70    3.90    2.10       4     .00    2.60    5.50    1.00
    2.90    5.30    3.70       6    1.00    2.40    5.30     .00
    1.00    3.70    1.70      10     .00    2.20    3.80     .00
    1.50    2.60     .30       8     .00    1.70    4.30     .00
    3.10    4.70    1.80       5     .00    2.10    4.90     .00
    4.20    5.10    2.70       3    1.00    1.30    5.30    1.00
    3.50    4.00    1.00       2     .00    2.40    4.90    1.00
    3.40    5.20    3.20       6    1.00    3.80    5.50     .00
    4.10    2.00    1.20      11     .00    2.40    3.90     .00
    2.70    3.60    1.70       6     .00    1.10    3.10     .00
    4.30    5.10    3.00       6    1.00    2.00    4.60     .00
    3.70    4.60    1.90       4     .00    4.10    4.30    1.00
    5.00    3.70    1.20       6     .00     .60    5.40     .00
    3.60    5.50    2.60       9    1.00    3.00    4.90     .00
    3.90    3.50     .70       5     .00    3.00    5.40    1.00
    2.60    4.40    1.50       3     .00    1.90    5.90    1.00
    2.50    3.90    2.20       2     .00    4.20    3.90    1.00
    2.90    2.30     .20       4     .00    1.00    4.00    1.00
    2.60    2.20    1.60      16     .00    2.80    4.10     .00
    2.70    4.40    1.40       5     .00    1.20    2.90     .00
    3.80    2.80    1.20       5     .00    2.40    2.90     .00
    3.20    5.70    1.60       8     .00    1.40    4.60     .00
    3.30    2.80    1.40       3     .00    2.40    3.50    1.00
    2.90    2.60    2.20       9     .00    3.00    3.30     .00
    1.80    1.50     .40      10     .00    2.50    3.40     .00
    3.50    2.40    1.40       5     .00    2.90    3.00     .00
    4.60    2.20     .00       8     .00    1.90    3.90     .00
    2.90    4.30    2.30       3     .00    3.60    5.00    1.00
    4.10    4.70     .40       4     .00    2.10    5.10    1.00
    2.40    3.60    2.60      13    1.00    2.20    3.70     .00
    5.30    6.40    1.50       5     .00    2.60    5.20     .00
    4.40    2.80    1.30       3     .00    1.70    5.00    1.00
    4.00    1.80    1.90       9     .00    5.90    4.10     .00
    6.20    2.90    3.10      11    1.00    3.10    3.60     .00
    2.30    3.80    1.00       8     .00    1.80    4.20     .00
    4.20    2.50    1.50      16     .00    1.00    2.60     .00
    2.40    3.50    1.40       5     .00    3.50    5.00     .00
    3.30    4.20    1.80       9     .00    3.20    3.40     .00
    3.10    2.60    2.70       2    1.00     .60    4.40    1.00
    2.50    4.50    1.20       2     .00    1.90    6.50    1.00
    2.50    2.80    2.60      12    1.00    3.40    2.20     .00
    3.70    1.80    1.20       8     .00    3.60    3.60     .00
    3.90    4.20    1.20       3     .00    3.00    5.00    1.00
    2.80    3.30    2.70       7    1.00     .90    4.70     .00
    3.80    5.20    1.90       3     .00    2.80    5.20    1.00
    5.10    5.00    2.40       8     .00    2.50    4.70     .00
    1.70    3.70    2.90       4    1.00    2.40    5.70    1.00
    3.00    4.10    2.10       3     .00    4.30    4.40    1.00
    3.50    4.70    2.30       4     .00    1.90    4.30    1.00
    5.30    6.80    1.40       3     .00    3.30    5.20    1.00
    2.80    4.90    1.80      11     .00    1.90    5.80     .00
    3.50    4.60    1.40       5     .00    2.30    4.60     .00
    3.30    5.10    4.30       8    1.00    3.60    4.70     .00
    2.70    3.60    1.60       5     .00    2.90    5.80     .00
    2.50    4.80    2.30       3     .00    2.70    7.00    1.00
    3.00    4.80    1.90       3     .00    3.40    5.50    1.00
    4.30    5.10    4.50       5    1.00    4.50    5.80     .00
    2.90    5.40     .80       2     .00    2.70    6.10    1.00
    2.40    2.80    2.00       4     .00    2.90    5.30    1.00
    4.30    3.90    2.30       6     .00    1.90    4.30     .00
    4.60    1.80     .00       9     .00    1.50    5.50     .00
    1.20    2.90    1.00       5     .00    1.10    3.70     .00
    4.10    3.70    1.80      13     .00    1.30    4.00     .00
    3.10    3.20    1.80       6     .00    3.40    4.70     .00
    1.90    4.50    1.40       7     .00    2.00    4.60     .00
    2.80    2.90    2.50      11     .00    2.90    1.20     .00
    1.00    2.90    2.10       2     .00    2.30    3.30    1.00
    3.30    4.90    2.10       8     .00    2.60    5.40     .00
    3.90    4.70    3.10      11    1.00    3.90    5.50     .00
    3.90    4.10    1.40       4     .00    3.00    4.30    1.00
    3.30    5.40    1.60       4     .00    2.50    6.10    1.00
    4.20    4.10    5.00       5    1.00    2.00    3.80    1.00
    4.20    4.70    1.40       6     .00    2.00    5.50     .00
    2.80    3.10    1.70      11     .00    1.80    2.10     .00
    3.20    5.70    1.30       3     .00    2.50    6.00    1.00
    3.90    5.00    1.60       4     .00    1.70    4.60    1.00
    2.20    3.90    1.50       1     .00    4.00    6.60    1.00
    3.60    4.30    3.30       1    1.00    2.10    4.10    1.00
    3.30    4.50    1.70       6     .00    1.30    5.20     .00
    5.30    5.60    3.70       5    1.00    2.70    5.20     .00
    1.60    4.10    3.80       3    1.00    2.80    4.90    1.00
    1.90    3.00    2.40       4     .00    1.50    4.00    1.00
    5.50    4.80    2.60       3    1.00    2.40    4.70    1.00
    3.80    2.20    1.20       7     .00    2.10    3.00     .00
    3.60    5.10    2.40       4     .00    1.20    4.00    1.00
    3.50    4.10    1.50       9     .00    1.30    3.30     .00
    2.20    5.20     .40       4     .00     .00    5.50    1.00
    4.50    4.80    3.20       6    1.00    3.20    5.80     .00
    2.10    5.20    3.00       7    1.00    2.90    4.60     .00
    1.90    3.80    3.10       2    1.00    2.00    5.70    1.00
    4.90    4.60    2.20       3     .00    2.80    6.90    1.00
    2.70    4.70    2.30       3     .00    3.50    3.60    1.00
    4.70    5.50    1.20       1     .00    1.90    5.60    1.00
    4.20    6.60    2.70       2    1.00     .60    4.30    1.00
    3.50    4.30    2.20       8     .00    2.80    4.80     .00
    3.20    4.00    2.00       6     .00    1.00    4.20     .00
    3.00    3.90    1.00       6     .00    1.30    3.20     .00
    3.50    5.20    1.90       2     .00    1.50    5.20    1.00
    2.30    5.00    1.70       7     .00    1.70    3.90     .00
    3.10    1.70    1.50       6     .00    2.40    2.30     .00
    3.80    5.30    3.00       5    1.00    3.00    5.50     .00
    2.40    3.90    2.70       3    1.00    2.30    5.40    1.00
    2.30    4.40    1.40       6     .00    2.00    3.50     .00
    3.00    3.50    2.70       5    1.00    3.20    3.10     .00
    3.60    2.50     .70       8     .00    2.20    2.90     .00
    3.30    2.50     .20       6     .00     .50    3.00     .00
    3.40    4.10    1.40       5     .00    1.80    4.90     .00
    2.60    4.40    1.60       2     .00    2.70    4.40    1.00
    2.60    3.20    3.00       8    1.00    1.60    2.40     .00
    2.80    3.70     .90       4     .00    2.00    3.40    1.00
    4.70    5.10     .00       4     .00     .30    3.70    1.00
    2.80    4.30    2.00       2     .00    1.30    2.70    1.00
    3.30    3.80    1.10       3     .00    2.60    5.90    1.00
    4.00    4.70    4.10       5    1.00    4.10    5.30    1.00
    3.50    4.30     .80       2     .00    2.00    4.60    1.00
    3.00    3.10     .80       1     .00    3.50    5.40    1.00
    3.50    2.60     .20       6     .00     .70    3.10     .00
    3.20    3.30    1.80       8     .00    4.00    5.60     .00
    4.70    6.20    2.50       2     .00    2.10    5.70    1.00
    3.10    4.20    2.60       8    1.00    2.60    3.60     .00
    1.70    2.30     .00       5     .00     .50    4.40     .00
    3.60    4.40    1.80      10     .00    1.50    4.50     .00
    5.10    6.30    1.90       4     .00    4.30    5.60    1.00
    4.30    3.10    1.70      11     .00    3.00    2.90     .00
    1.60    4.20    1.80       8     .00    1.60    2.70     .00
    2.40    3.50     .90       4     .00    3.00    4.00    1.00
    2.80    3.90    2.00      10     .00    2.50    2.80     .00
    3.00    4.50    3.00       2    1.00    1.60    5.10    1.00
    2.80    3.30    2.20       2     .00    2.30    4.60    1.00
    2.50    2.10    2.40       9     .00    3.30    3.70     .00
    3.30    4.40    2.50       3     .00    2.50    4.60    1.00
    2.80    2.50    1.30       9     .00    3.30    3.90     .00
    4.80    3.10    3.20       3    1.00    3.30    2.80    1.00
    1.50    2.30     .00      16     .00    1.10    3.00     .00
    2.40    5.10     .90       2     .00    1.80    5.60    1.00
    4.60    4.10    2.60       3    1.00    3.50    4.50    1.00
    4.50    5.00     .60       2     .00    1.70    6.50    1.00
    3.50    5.80     .70       4     .00    1.20    5.40    1.00
    3.00    3.60    1.70       2     .00    3.80    4.00    1.00
    4.50    4.80    1.30       4     .00    2.70    4.50    1.00
    3.60    5.00    2.00       3     .00    2.00    5.00    1.00
    5.00    5.20    1.50       4     .00    2.90    3.90    1.00
    3.60    4.30    1.30       4     .00    1.30    5.50    1.00
    2.30    5.10     .00       3     .00    2.70    5.70    1.00
    3.00    4.50    1.90       6     .00     .70    5.40     .00
    2.80    4.20     .60      10     .00    1.30    3.50     .00
    3.10    2.60     .50       2     .00    2.40    2.10    1.00
    2.20    4.30    2.10      14     .00    1.20    3.70     .00
    4.20    6.10    3.60       3    1.00    3.20    5.00    1.00
    2.70    4.10    2.30       9     .00    3.80    5.60     .00
    3.70    5.90    2.50       2     .00    4.30    6.30    1.00
    2.30    4.50    3.10      10    1.00    2.20    2.90     .00
    2.70    4.60    1.80       8     .00    3.40    3.30     .00
    3.80    5.40    1.60      10     .00    1.60    3.60     .00
    4.10    6.50    2.20       5     .00    2.50    5.40     .00
    4.40    4.80    1.60       2     .00    2.30    4.60    1.00
    3.90    4.20    1.70       4     .00    3.00    3.80    1.00
    3.70    1.60     .10       7     .00    1.00    2.90     .00
    4.40    5.10    2.40       5     .00    2.60    5.70    1.00
    4.60    4.50    2.30       1     .00    2.40    5.50    1.00
    2.40    3.20    1.40       3     .00    2.50    3.20    1.00
    2.00    4.30    1.90      12     .00    1.60    3.60     .00
    2.80    2.80    1.80       5     .00    2.00    4.90    1.00
    4.00    3.80    2.70      11    1.00    2.50    4.60     .00
    3.20    2.80    2.10       5     .00    2.10    2.90     .00
    4.70    3.90    2.80       7    1.00    2.00    5.10     .00
    4.00    6.20     .00       1     .00    3.00    5.30    1.00
    4.20    4.60    2.00       4     .00     .90    5.40    1.00
    3.20    4.40    2.50       3     .00    1.70    3.20    1.00
    3.60    4.10    3.30      10    1.00    3.70    4.30     .00
    3.00    3.80    2.20      11     .00    3.70    5.60     .00
    3.50    3.90    1.40       2     .00     .60    3.40    1.00
    1.90    4.50    2.80       8    1.00    4.10    6.00     .00
    2.10    2.60     .40       1     .00    4.20    4.30    1.00
    4.10    3.50    1.50       8     .00    2.70    5.00     .00
    4.00    2.10     .90       7     .00    1.90    3.80     .00
    4.40    3.40    2.30       5     .00    2.30    4.30    1.00
    3.40    4.60    1.60       7     .00    1.70    6.30     .00
    4.10    4.80     .90       2     .00     .80    4.50    1.00
    3.90    5.30    1.90       3     .00    3.30    5.30    1.00
    2.90    4.00    2.60       3    1.00    2.80    3.70    1.00
    3.10    5.80    2.20       3     .00    3.20    5.10    1.00
    4.80    4.30    2.00       3     .00    3.60    5.80    1.00
    3.70    2.80    2.90       4    1.00    1.90    4.40    1.00
    4.00    5.30     .00       5     .00     .60    5.80     .00
    3.60    4.10    2.40       5     .00     .80    3.10     .00
    3.20    5.00    2.60       5    1.00    1.20    4.80     .00
    2.60    3.80    2.80       4    1.00    3.60    6.40    1.00
    3.60    4.90    3.90       5    1.00    4.40    4.00     .00
    4.10    3.70     .90       7     .00    1.80    5.70     .00
    2.80    4.10    4.00       3    1.00    1.30    4.60    1.00
    4.90    4.40    4.20       8    1.00    5.30    4.70     .00
    5.00    5.20    1.90       5     .00    1.20    5.60     .00
    1.00    4.60    3.20       6    1.00    2.90    4.20     .00
    3.70    5.30    2.50       3     .00    2.20    5.50    1.00
    3.20    3.30    1.50      11     .00    1.40    4.10     .00
    2.80    2.80    2.40       6     .00    2.30    3.60     .00
    1.90    5.50    2.50       2     .00    4.20    6.30    1.00
    2.90    4.20    2.00       2     .00     .90    3.50    1.00
    2.90    4.10    2.30       6     .00    2.80    3.30     .00
    4.00    3.90    2.40      16     .00    1.10    3.70     .00
    2.20    4.70    2.40       2     .00    1.40    4.50    1.00
    4.10    4.40    3.60       5    1.00     .80    2.30     .00
    5.60    3.20    1.80       7     .00    1.80    3.50     .00
    3.50    5.20    3.30       3    1.00    2.00    3.80    1.00
    2.90    4.50    1.20       7     .00    2.70    4.50     .00
    3.00    3.30    2.80       3    1.00    2.50    5.10    1.00
    5.40    5.00    3.00      10    1.00    3.70    5.50     .00
    4.40    6.50    2.00       3     .00    1.00    5.30    1.00
    3.60    4.60    2.70       3    1.00    2.00    4.20    1.00
    3.80    5.30    1.70       1     .00    2.80    7.00    1.00
    3.80    4.70    3.40       2    1.00    2.50    4.20    1.00
	run;
