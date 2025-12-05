# Gravity Falls Anomaly Detection 

## Citation 

```
@ARTICLE{11141690,
  author={Kirsch, Maximilian and Wernicke, Jakob and Datta, Pawanjeet Singh and Preisach, Christine},
  journal={IEEE Journal of Selected Topics in Applied Earth Observations and Remote Sensing}, 
  title={Early Detection of Forest Disturbances in Homogeneous Stands – Deep Learning-Based Monitoring of Picea Abies}, 
  year={2025},
  volume={18},
  number={},
  pages={22300-22316},
  keywords={Climate change;Remote sensing;Insects;Autoencoders;Forestry;Climate change;Anomaly detection;Anomaly detection;autoencoder;bark beetle;forest;long short term memory (LSTM);remote sensing;spruce},
  doi={10.1109/JSTARS.2025.3603094}}

```

## Abstract

Climate change has increased the vulnerability of forests to insect-related damage, resulting in widespread forest loss in Central Europe and highlighting the need for effective, continuous monitoring systems. Remote sensing-based approaches, often rely on supervised machine learning (ML) algorithms that require labeled training data. Monitoring temporal patterns through time series analysis offers a potential alternative for earlier detection of disturbances but requires substantial storage resources. This study investigates the potential of a Deep Learning algorithm based on a Long Short Term Memory (LSTM) Autoencoder for the detection of anomalies in forest health (e.g., bark beetle outbreaks), utilizing Sentinel-2 time series data. This approach is an alternative to supervised ML methods, avoiding the necessity for labeled training data. Furthermore, it is more memory-efficient than other time series analysis approaches, as a robust model can be created using only a 26-week-long time series as input. In this study, we monitored pure stands of spruce in Thuringia, Germany, over a 7-year period from 2018 to the end of 2024. Our best model achieved a detection accuracy of 87% on test data and was able to detect 65% of all anomalies at a very early stage (more than a month before visible signs of forest disturbance). Compared to the established time series break detection algorithm – breaks for additive season and trend and isolation forest anomaly detection, our approach consistently detected a higher percentage of anomalies at an earlier stage. These findings suggest that LSTM-based Autoencoders could provide a promising, resource-efficient approach to forest health monitoring, enabling more timely responses to emerging threats.