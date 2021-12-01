{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AutoScaling.Types.LaunchConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.LaunchConfiguration where

import Amazonka.AutoScaling.Types.BlockDeviceMapping
import Amazonka.AutoScaling.Types.InstanceMetadataOptions
import Amazonka.AutoScaling.Types.InstanceMonitoring
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a launch configuration.
--
-- /See:/ 'newLaunchConfiguration' smart constructor.
data LaunchConfiguration = LaunchConfiguration'
  { -- | For Auto Scaling groups that are running in a VPC, specifies whether to
    -- assign a public IP address to the group\'s instances. For more
    -- information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | A list that contains the security groups to assign to the instances in
    -- the Auto Scaling group. For more information, see
    -- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC>
    -- in the /Amazon Virtual Private Cloud User Guide/.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The maximum hourly price to be paid for any Spot Instance launched to
    -- fulfill the request. Spot Instances are launched when the price you
    -- specify exceeds the current Spot price. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | Controls whether instances in this group are launched with detailed
    -- (@true@) or basic (@false@) monitoring.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    instanceMonitoring :: Prelude.Maybe InstanceMonitoring,
    -- | The name of the key pair.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The IDs of one or more security groups for the VPC specified in
    -- @ClassicLinkVPCId@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
    -- in the /Amazon EC2 User Guide for Linux Instances/ and
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    classicLinkVPCSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the RAM disk associated with the AMI.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the kernel associated with the AMI.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the launch configuration is optimized for EBS I\/O
    -- (@true@) or not (@false@). For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The user data to make available to the launched EC2 instances. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
    -- (Linux) and
    -- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html Instance metadata and user data>
    -- (Windows). If you are using a command line tool, base64-encoding is
    -- performed for you, and you can load the text from a file. Otherwise, you
    -- must provide base64-encoded text. User data is limited to 16 KB.
    userData :: Prelude.Maybe Prelude.Text,
    -- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances
    -- to. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
    -- in the /Amazon EC2 User Guide for Linux Instances/ and
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    classicLinkVPCId :: Prelude.Maybe Prelude.Text,
    -- | The name or the Amazon Resource Name (ARN) of the instance profile
    -- associated with the IAM role for the instance. The instance profile
    -- contains the IAM role. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    iamInstanceProfile :: Prelude.Maybe Prelude.Text,
    -- | The metadata options for the instances. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    metadataOptions :: Prelude.Maybe InstanceMetadataOptions,
    -- | The Amazon Resource Name (ARN) of the launch configuration.
    launchConfigurationARN :: Prelude.Maybe Prelude.Text,
    -- | The tenancy of the instance, either @default@ or @dedicated@. An
    -- instance with @dedicated@ tenancy runs on isolated, single-tenant
    -- hardware and can only be launched into a VPC.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    placementTenancy :: Prelude.Maybe Prelude.Text,
    -- | A block device mapping, which specifies the block devices for the
    -- instance. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | The name of the launch configuration.
    launchConfigurationName :: Prelude.Text,
    -- | The ID of the Amazon Machine Image (AMI) to use to launch your EC2
    -- instances. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    imageId :: Prelude.Text,
    -- | The instance type for the instances.
    --
    -- For information about available instance types, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    instanceType :: Prelude.Text,
    -- | The creation date and time for the launch configuration.
    createdTime :: Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatePublicIpAddress', 'launchConfiguration_associatePublicIpAddress' - For Auto Scaling groups that are running in a VPC, specifies whether to
-- assign a public IP address to the group\'s instances. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'securityGroups', 'launchConfiguration_securityGroups' - A list that contains the security groups to assign to the instances in
-- the Auto Scaling group. For more information, see
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- 'spotPrice', 'launchConfiguration_spotPrice' - The maximum hourly price to be paid for any Spot Instance launched to
-- fulfill the request. Spot Instances are launched when the price you
-- specify exceeds the current Spot price. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'instanceMonitoring', 'launchConfiguration_instanceMonitoring' - Controls whether instances in this group are launched with detailed
-- (@true@) or basic (@false@) monitoring.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'keyName', 'launchConfiguration_keyName' - The name of the key pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'classicLinkVPCSecurityGroups', 'launchConfiguration_classicLinkVPCSecurityGroups' - The IDs of one or more security groups for the VPC specified in
-- @ClassicLinkVPCId@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon EC2 User Guide for Linux Instances/ and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'ramdiskId', 'launchConfiguration_ramdiskId' - The ID of the RAM disk associated with the AMI.
--
-- 'kernelId', 'launchConfiguration_kernelId' - The ID of the kernel associated with the AMI.
--
-- 'ebsOptimized', 'launchConfiguration_ebsOptimized' - Specifies whether the launch configuration is optimized for EBS I\/O
-- (@true@) or not (@false@). For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'userData', 'launchConfiguration_userData' - The user data to make available to the launched EC2 instances. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
-- (Linux) and
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html Instance metadata and user data>
-- (Windows). If you are using a command line tool, base64-encoding is
-- performed for you, and you can load the text from a file. Otherwise, you
-- must provide base64-encoded text. User data is limited to 16 KB.
--
-- 'classicLinkVPCId', 'launchConfiguration_classicLinkVPCId' - The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances
-- to. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon EC2 User Guide for Linux Instances/ and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'iamInstanceProfile', 'launchConfiguration_iamInstanceProfile' - The name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance. The instance profile
-- contains the IAM role. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'metadataOptions', 'launchConfiguration_metadataOptions' - The metadata options for the instances. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'launchConfigurationARN', 'launchConfiguration_launchConfigurationARN' - The Amazon Resource Name (ARN) of the launch configuration.
--
-- 'placementTenancy', 'launchConfiguration_placementTenancy' - The tenancy of the instance, either @default@ or @dedicated@. An
-- instance with @dedicated@ tenancy runs on isolated, single-tenant
-- hardware and can only be launched into a VPC.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'blockDeviceMappings', 'launchConfiguration_blockDeviceMappings' - A block device mapping, which specifies the block devices for the
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'launchConfigurationName', 'launchConfiguration_launchConfigurationName' - The name of the launch configuration.
--
-- 'imageId', 'launchConfiguration_imageId' - The ID of the Amazon Machine Image (AMI) to use to launch your EC2
-- instances. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'instanceType', 'launchConfiguration_instanceType' - The instance type for the instances.
--
-- For information about available instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'createdTime', 'launchConfiguration_createdTime' - The creation date and time for the launch configuration.
newLaunchConfiguration ::
  -- | 'launchConfigurationName'
  Prelude.Text ->
  -- | 'imageId'
  Prelude.Text ->
  -- | 'instanceType'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  LaunchConfiguration
newLaunchConfiguration
  pLaunchConfigurationName_
  pImageId_
  pInstanceType_
  pCreatedTime_ =
    LaunchConfiguration'
      { associatePublicIpAddress =
          Prelude.Nothing,
        securityGroups = Prelude.Nothing,
        spotPrice = Prelude.Nothing,
        instanceMonitoring = Prelude.Nothing,
        keyName = Prelude.Nothing,
        classicLinkVPCSecurityGroups = Prelude.Nothing,
        ramdiskId = Prelude.Nothing,
        kernelId = Prelude.Nothing,
        ebsOptimized = Prelude.Nothing,
        userData = Prelude.Nothing,
        classicLinkVPCId = Prelude.Nothing,
        iamInstanceProfile = Prelude.Nothing,
        metadataOptions = Prelude.Nothing,
        launchConfigurationARN = Prelude.Nothing,
        placementTenancy = Prelude.Nothing,
        blockDeviceMappings = Prelude.Nothing,
        launchConfigurationName = pLaunchConfigurationName_,
        imageId = pImageId_,
        instanceType = pInstanceType_,
        createdTime = Core._Time Lens.# pCreatedTime_
      }

-- | For Auto Scaling groups that are running in a VPC, specifies whether to
-- assign a public IP address to the group\'s instances. For more
-- information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html Launching Auto Scaling instances in a VPC>
-- in the /Amazon EC2 Auto Scaling User Guide/.
launchConfiguration_associatePublicIpAddress :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Bool)
launchConfiguration_associatePublicIpAddress = Lens.lens (\LaunchConfiguration' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@LaunchConfiguration' {} a -> s {associatePublicIpAddress = a} :: LaunchConfiguration)

-- | A list that contains the security groups to assign to the instances in
-- the Auto Scaling group. For more information, see
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security Groups for Your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
launchConfiguration_securityGroups :: Lens.Lens' LaunchConfiguration (Prelude.Maybe [Prelude.Text])
launchConfiguration_securityGroups = Lens.lens (\LaunchConfiguration' {securityGroups} -> securityGroups) (\s@LaunchConfiguration' {} a -> s {securityGroups = a} :: LaunchConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The maximum hourly price to be paid for any Spot Instance launched to
-- fulfill the request. Spot Instances are launched when the price you
-- specify exceeds the current Spot price. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-launch-spot-instances.html Requesting Spot Instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
launchConfiguration_spotPrice :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_spotPrice = Lens.lens (\LaunchConfiguration' {spotPrice} -> spotPrice) (\s@LaunchConfiguration' {} a -> s {spotPrice = a} :: LaunchConfiguration)

-- | Controls whether instances in this group are launched with detailed
-- (@true@) or basic (@false@) monitoring.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/latest/userguide/enable-as-instance-metrics.html Configure Monitoring for Auto Scaling Instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
launchConfiguration_instanceMonitoring :: Lens.Lens' LaunchConfiguration (Prelude.Maybe InstanceMonitoring)
launchConfiguration_instanceMonitoring = Lens.lens (\LaunchConfiguration' {instanceMonitoring} -> instanceMonitoring) (\s@LaunchConfiguration' {} a -> s {instanceMonitoring = a} :: LaunchConfiguration)

-- | The name of the key pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Amazon EC2 Key Pairs>
-- in the /Amazon EC2 User Guide for Linux Instances/.
launchConfiguration_keyName :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_keyName = Lens.lens (\LaunchConfiguration' {keyName} -> keyName) (\s@LaunchConfiguration' {} a -> s {keyName = a} :: LaunchConfiguration)

-- | The IDs of one or more security groups for the VPC specified in
-- @ClassicLinkVPCId@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon EC2 User Guide for Linux Instances/ and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC>
-- in the /Amazon EC2 Auto Scaling User Guide/.
launchConfiguration_classicLinkVPCSecurityGroups :: Lens.Lens' LaunchConfiguration (Prelude.Maybe [Prelude.Text])
launchConfiguration_classicLinkVPCSecurityGroups = Lens.lens (\LaunchConfiguration' {classicLinkVPCSecurityGroups} -> classicLinkVPCSecurityGroups) (\s@LaunchConfiguration' {} a -> s {classicLinkVPCSecurityGroups = a} :: LaunchConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the RAM disk associated with the AMI.
launchConfiguration_ramdiskId :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_ramdiskId = Lens.lens (\LaunchConfiguration' {ramdiskId} -> ramdiskId) (\s@LaunchConfiguration' {} a -> s {ramdiskId = a} :: LaunchConfiguration)

-- | The ID of the kernel associated with the AMI.
launchConfiguration_kernelId :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_kernelId = Lens.lens (\LaunchConfiguration' {kernelId} -> kernelId) (\s@LaunchConfiguration' {} a -> s {kernelId = a} :: LaunchConfiguration)

-- | Specifies whether the launch configuration is optimized for EBS I\/O
-- (@true@) or not (@false@). For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
launchConfiguration_ebsOptimized :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Bool)
launchConfiguration_ebsOptimized = Lens.lens (\LaunchConfiguration' {ebsOptimized} -> ebsOptimized) (\s@LaunchConfiguration' {} a -> s {ebsOptimized = a} :: LaunchConfiguration)

-- | The user data to make available to the launched EC2 instances. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html Instance metadata and user data>
-- (Linux) and
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ec2-instance-metadata.html Instance metadata and user data>
-- (Windows). If you are using a command line tool, base64-encoding is
-- performed for you, and you can load the text from a file. Otherwise, you
-- must provide base64-encoded text. User data is limited to 16 KB.
launchConfiguration_userData :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_userData = Lens.lens (\LaunchConfiguration' {userData} -> userData) (\s@LaunchConfiguration' {} a -> s {userData = a} :: LaunchConfiguration)

-- | The ID of a ClassicLink-enabled VPC to link your EC2-Classic instances
-- to. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon EC2 User Guide for Linux Instances/ and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-in-vpc.html#as-ClassicLink Linking EC2-Classic instances to a VPC>
-- in the /Amazon EC2 Auto Scaling User Guide/.
launchConfiguration_classicLinkVPCId :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_classicLinkVPCId = Lens.lens (\LaunchConfiguration' {classicLinkVPCId} -> classicLinkVPCId) (\s@LaunchConfiguration' {} a -> s {classicLinkVPCId = a} :: LaunchConfiguration)

-- | The name or the Amazon Resource Name (ARN) of the instance profile
-- associated with the IAM role for the instance. The instance profile
-- contains the IAM role. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/us-iam-role.html IAM role for applications that run on Amazon EC2 instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
launchConfiguration_iamInstanceProfile :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_iamInstanceProfile = Lens.lens (\LaunchConfiguration' {iamInstanceProfile} -> iamInstanceProfile) (\s@LaunchConfiguration' {} a -> s {iamInstanceProfile = a} :: LaunchConfiguration)

-- | The metadata options for the instances. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-launch-config.html#launch-configurations-imds Configuring the Instance Metadata Options>
-- in the /Amazon EC2 Auto Scaling User Guide/.
launchConfiguration_metadataOptions :: Lens.Lens' LaunchConfiguration (Prelude.Maybe InstanceMetadataOptions)
launchConfiguration_metadataOptions = Lens.lens (\LaunchConfiguration' {metadataOptions} -> metadataOptions) (\s@LaunchConfiguration' {} a -> s {metadataOptions = a} :: LaunchConfiguration)

-- | The Amazon Resource Name (ARN) of the launch configuration.
launchConfiguration_launchConfigurationARN :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_launchConfigurationARN = Lens.lens (\LaunchConfiguration' {launchConfigurationARN} -> launchConfigurationARN) (\s@LaunchConfiguration' {} a -> s {launchConfigurationARN = a} :: LaunchConfiguration)

-- | The tenancy of the instance, either @default@ or @dedicated@. An
-- instance with @dedicated@ tenancy runs on isolated, single-tenant
-- hardware and can only be launched into a VPC.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/auto-scaling-dedicated-instances.html Configuring instance tenancy with Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
launchConfiguration_placementTenancy :: Lens.Lens' LaunchConfiguration (Prelude.Maybe Prelude.Text)
launchConfiguration_placementTenancy = Lens.lens (\LaunchConfiguration' {placementTenancy} -> placementTenancy) (\s@LaunchConfiguration' {} a -> s {placementTenancy = a} :: LaunchConfiguration)

-- | A block device mapping, which specifies the block devices for the
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html Block Device Mapping>
-- in the /Amazon EC2 User Guide for Linux Instances/.
launchConfiguration_blockDeviceMappings :: Lens.Lens' LaunchConfiguration (Prelude.Maybe [BlockDeviceMapping])
launchConfiguration_blockDeviceMappings = Lens.lens (\LaunchConfiguration' {blockDeviceMappings} -> blockDeviceMappings) (\s@LaunchConfiguration' {} a -> s {blockDeviceMappings = a} :: LaunchConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the launch configuration.
launchConfiguration_launchConfigurationName :: Lens.Lens' LaunchConfiguration Prelude.Text
launchConfiguration_launchConfigurationName = Lens.lens (\LaunchConfiguration' {launchConfigurationName} -> launchConfigurationName) (\s@LaunchConfiguration' {} a -> s {launchConfigurationName = a} :: LaunchConfiguration)

-- | The ID of the Amazon Machine Image (AMI) to use to launch your EC2
-- instances. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/finding-an-ami.html Finding an AMI>
-- in the /Amazon EC2 User Guide for Linux Instances/.
launchConfiguration_imageId :: Lens.Lens' LaunchConfiguration Prelude.Text
launchConfiguration_imageId = Lens.lens (\LaunchConfiguration' {imageId} -> imageId) (\s@LaunchConfiguration' {} a -> s {imageId = a} :: LaunchConfiguration)

-- | The instance type for the instances.
--
-- For information about available instance types, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#AvailableInstanceTypes Available Instance Types>
-- in the /Amazon EC2 User Guide for Linux Instances/.
launchConfiguration_instanceType :: Lens.Lens' LaunchConfiguration Prelude.Text
launchConfiguration_instanceType = Lens.lens (\LaunchConfiguration' {instanceType} -> instanceType) (\s@LaunchConfiguration' {} a -> s {instanceType = a} :: LaunchConfiguration)

-- | The creation date and time for the launch configuration.
launchConfiguration_createdTime :: Lens.Lens' LaunchConfiguration Prelude.UTCTime
launchConfiguration_createdTime = Lens.lens (\LaunchConfiguration' {createdTime} -> createdTime) (\s@LaunchConfiguration' {} a -> s {createdTime = a} :: LaunchConfiguration) Prelude.. Core._Time

instance Core.FromXML LaunchConfiguration where
  parseXML x =
    LaunchConfiguration'
      Prelude.<$> (x Core..@? "AssociatePublicIpAddress")
      Prelude.<*> ( x Core..@? "SecurityGroups" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "SpotPrice")
      Prelude.<*> (x Core..@? "InstanceMonitoring")
      Prelude.<*> (x Core..@? "KeyName")
      Prelude.<*> ( x Core..@? "ClassicLinkVPCSecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "RamdiskId")
      Prelude.<*> (x Core..@? "KernelId")
      Prelude.<*> (x Core..@? "EbsOptimized")
      Prelude.<*> (x Core..@? "UserData")
      Prelude.<*> (x Core..@? "ClassicLinkVPCId")
      Prelude.<*> (x Core..@? "IamInstanceProfile")
      Prelude.<*> (x Core..@? "MetadataOptions")
      Prelude.<*> (x Core..@? "LaunchConfigurationARN")
      Prelude.<*> (x Core..@? "PlacementTenancy")
      Prelude.<*> ( x Core..@? "BlockDeviceMappings"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@ "LaunchConfigurationName")
      Prelude.<*> (x Core..@ "ImageId")
      Prelude.<*> (x Core..@ "InstanceType")
      Prelude.<*> (x Core..@ "CreatedTime")

instance Prelude.Hashable LaunchConfiguration where
  hashWithSalt salt' LaunchConfiguration' {..} =
    salt' `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` launchConfigurationName
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` placementTenancy
      `Prelude.hashWithSalt` launchConfigurationARN
      `Prelude.hashWithSalt` metadataOptions
      `Prelude.hashWithSalt` iamInstanceProfile
      `Prelude.hashWithSalt` classicLinkVPCId
      `Prelude.hashWithSalt` userData
      `Prelude.hashWithSalt` ebsOptimized
      `Prelude.hashWithSalt` kernelId
      `Prelude.hashWithSalt` ramdiskId
      `Prelude.hashWithSalt` classicLinkVPCSecurityGroups
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` instanceMonitoring
      `Prelude.hashWithSalt` spotPrice
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` associatePublicIpAddress

instance Prelude.NFData LaunchConfiguration where
  rnf LaunchConfiguration' {..} =
    Prelude.rnf associatePublicIpAddress
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf launchConfigurationName
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf placementTenancy
      `Prelude.seq` Prelude.rnf launchConfigurationARN
      `Prelude.seq` Prelude.rnf metadataOptions
      `Prelude.seq` Prelude.rnf iamInstanceProfile
      `Prelude.seq` Prelude.rnf classicLinkVPCId
      `Prelude.seq` Prelude.rnf userData
      `Prelude.seq` Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf ramdiskId
      `Prelude.seq` Prelude.rnf classicLinkVPCSecurityGroups
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf instanceMonitoring
      `Prelude.seq` Prelude.rnf spotPrice
      `Prelude.seq` Prelude.rnf securityGroups
