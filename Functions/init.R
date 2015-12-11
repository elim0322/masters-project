# ----------
# Read data
# ----------
dat1 = read.csv("../kddcup.data_10_percent_corrected",  header = FALSE)
# dat2 = read.csv("../kddcup.testdata_10_percent_corrected", header = FALSE)

## features
names(dat1)  = c("duration", "protocol_type", "service", "flag", "src_bytes", "dst_bytes", "land", "wrong_fragment", "urgent", "hot", "num_failed_logins", "logged_in", "num_compromised", "root_shell", "su_attempted", "num_root", "num_file_creations", "num_shells", "num_access_files", "num_outbound_cmds", "is_host_login", "is_guest_login", "count", "srv_count", "serror_rate", "srv_serror_rate", "rerror_rate", "srv_rerror_rate", "same_srv_rate", "diff_srv_rate", "srv_diff_host_rate", "dst_host_count", "dst_host_srv_count", "dst_host_same_srv_rate", "dst_host_diff_srv_rate", "dst_host_same_src_port_rate", "dst_host_srv_diff_host_rate", "dst_host_serror_rate", "dst_host_srv_serror_rate", "dst_host_rerror_rate", "dst_host_srv_rerror_rate", "attack_type")
# names(dat2) = names(dat1)

## number of duplicates
# sum(duplicated(dat1))
# sum(duplicated(dat2))

## remove duplicates
# dat1.undup = dat1[!duplicated(dat1), ]
# dat2.undup = dat2[!duplicated(dat2), ]


# -------------
# Set up RWeka
# -------------
## Set RWeka options
options(java.parameters = "-Xmx4g") # heap size = 4g
library(RWeka)
Sys.setenv(WEKA_HOME = "C:\\Users\\OEM\\wekafiles")

## Load Weka packages
WPM("load-package", "localOutlierFactor")
# WPM("load-package", "XMeans")
WPM("load-package", "optics_dbScan")

## Make Weka filters
LOF = make_Weka_filter("weka/filters/unsupervised/attribute/LOF")
DBScan = make_Weka_clusterer("weka/clusterers/DBSCAN")

# install packages
# WPM("install-package", "localOutlierFactor")
# WPM("install-package", "XMeans")
# WPM("install-package", "optics_dbScan")

