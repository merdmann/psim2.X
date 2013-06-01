#!/bin/sh 
gcutil --service_version="v1beta15" \
   --project="sage-webbing-230" \
   addinstance --zone="europe-west1-a" \
   --machine_type="n1-highcpu-8" \
   --network="default" \
   --external_ip_address="ephemeral" \
   --service_account_scopes="https://www.googleapis.com/auth/userinfo.email,https://www.googleapis.com/auth/compute,https://www.googleapis.com/auth/devstorage.full_control" \
   --disk="rootfs,deviceName=rootfs,mode=READ_WRITE,boot" \
   --kernel="https://www.googleapis.com/compute/v1beta15/projects/google/global/kernels/gce-v20130522" myspace 

ip=`gcutil listinstances --zone="europe-west1-a" | grep myspace | cut -d '|' -f 7`
echo "$ip is ready"



