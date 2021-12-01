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
-- Module      : Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails
import Amazonka.SecurityHub.Types.AwsAutoScalingLaunchConfigurationInstanceMonitoringDetails

-- | Details about a launch configuration.
--
-- /See:/ 'newAwsAutoScalingLaunchConfigurationDetails' smart constructor.
data AwsAutoScalingLaunchConfigurationDetails = AwsAutoScalingLaunchConfigurationDetails'
  { -- | For Auto Scaling groups that run in a VPC, specifies whether to assign a
    -- public IP address to the group\'s instances.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | The security groups to assign to the instances in the Auto Scaling
    -- group.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The maximum hourly price to be paid for any Spot Instance that is
    -- launched to fulfill the request.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | The creation date and time for the launch configuration.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | Indicates the type of monitoring for instances in the group.
    instanceMonitoring :: Prelude.Maybe AwsAutoScalingLaunchConfigurationInstanceMonitoringDetails,
    -- | The name of the key pair.
    keyName :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of one or more security groups for the VPC that is
    -- specified in @ClassicLinkVPCId@.
    classicLinkVpcSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the RAM disk associated with the AMI.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the kernel associated with the AMI.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The instance type for the instances.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | Whether the launch configuration is optimized for Amazon EBS I\/O.
    ebsOptimized :: Prelude.Maybe Prelude.Bool,
    -- | The user data to make available to the launched EC2 instances. Must be
    -- base64-encoded text.
    userData :: Prelude.Maybe Prelude.Text,
    -- | The identifier of a ClassicLink-enabled VPC that EC2-Classic instances
    -- are linked to.
    classicLinkVpcId :: Prelude.Maybe Prelude.Text,
    -- | The name or the ARN of the instance profile associated with the IAM role
    -- for the instance. The instance profile contains the IAM role.
    iamInstanceProfile :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Machine Image (AMI) that is used to launch
    -- EC2 instances.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch configuration.
    launchConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The tenancy of the instance. An instance with @dedicated@ tenancy runs
    -- on isolated, single-tenant hardware and can only be launched into a VPC.
    placementTenancy :: Prelude.Maybe Prelude.Text,
    -- | Specifies the block devices for the instance.
    blockDeviceMappings :: Prelude.Maybe [AwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAutoScalingLaunchConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatePublicIpAddress', 'awsAutoScalingLaunchConfigurationDetails_associatePublicIpAddress' - For Auto Scaling groups that run in a VPC, specifies whether to assign a
-- public IP address to the group\'s instances.
--
-- 'securityGroups', 'awsAutoScalingLaunchConfigurationDetails_securityGroups' - The security groups to assign to the instances in the Auto Scaling
-- group.
--
-- 'spotPrice', 'awsAutoScalingLaunchConfigurationDetails_spotPrice' - The maximum hourly price to be paid for any Spot Instance that is
-- launched to fulfill the request.
--
-- 'createdTime', 'awsAutoScalingLaunchConfigurationDetails_createdTime' - The creation date and time for the launch configuration.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'instanceMonitoring', 'awsAutoScalingLaunchConfigurationDetails_instanceMonitoring' - Indicates the type of monitoring for instances in the group.
--
-- 'keyName', 'awsAutoScalingLaunchConfigurationDetails_keyName' - The name of the key pair.
--
-- 'classicLinkVpcSecurityGroups', 'awsAutoScalingLaunchConfigurationDetails_classicLinkVpcSecurityGroups' - The identifiers of one or more security groups for the VPC that is
-- specified in @ClassicLinkVPCId@.
--
-- 'ramdiskId', 'awsAutoScalingLaunchConfigurationDetails_ramdiskId' - The identifier of the RAM disk associated with the AMI.
--
-- 'kernelId', 'awsAutoScalingLaunchConfigurationDetails_kernelId' - The identifier of the kernel associated with the AMI.
--
-- 'instanceType', 'awsAutoScalingLaunchConfigurationDetails_instanceType' - The instance type for the instances.
--
-- 'ebsOptimized', 'awsAutoScalingLaunchConfigurationDetails_ebsOptimized' - Whether the launch configuration is optimized for Amazon EBS I\/O.
--
-- 'userData', 'awsAutoScalingLaunchConfigurationDetails_userData' - The user data to make available to the launched EC2 instances. Must be
-- base64-encoded text.
--
-- 'classicLinkVpcId', 'awsAutoScalingLaunchConfigurationDetails_classicLinkVpcId' - The identifier of a ClassicLink-enabled VPC that EC2-Classic instances
-- are linked to.
--
-- 'iamInstanceProfile', 'awsAutoScalingLaunchConfigurationDetails_iamInstanceProfile' - The name or the ARN of the instance profile associated with the IAM role
-- for the instance. The instance profile contains the IAM role.
--
-- 'imageId', 'awsAutoScalingLaunchConfigurationDetails_imageId' - The identifier of the Amazon Machine Image (AMI) that is used to launch
-- EC2 instances.
--
-- 'launchConfigurationName', 'awsAutoScalingLaunchConfigurationDetails_launchConfigurationName' - The name of the launch configuration.
--
-- 'placementTenancy', 'awsAutoScalingLaunchConfigurationDetails_placementTenancy' - The tenancy of the instance. An instance with @dedicated@ tenancy runs
-- on isolated, single-tenant hardware and can only be launched into a VPC.
--
-- 'blockDeviceMappings', 'awsAutoScalingLaunchConfigurationDetails_blockDeviceMappings' - Specifies the block devices for the instance.
newAwsAutoScalingLaunchConfigurationDetails ::
  AwsAutoScalingLaunchConfigurationDetails
newAwsAutoScalingLaunchConfigurationDetails =
  AwsAutoScalingLaunchConfigurationDetails'
    { associatePublicIpAddress =
        Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      spotPrice = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      instanceMonitoring =
        Prelude.Nothing,
      keyName = Prelude.Nothing,
      classicLinkVpcSecurityGroups =
        Prelude.Nothing,
      ramdiskId = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      ebsOptimized = Prelude.Nothing,
      userData = Prelude.Nothing,
      classicLinkVpcId =
        Prelude.Nothing,
      iamInstanceProfile =
        Prelude.Nothing,
      imageId = Prelude.Nothing,
      launchConfigurationName =
        Prelude.Nothing,
      placementTenancy =
        Prelude.Nothing,
      blockDeviceMappings =
        Prelude.Nothing
    }

-- | For Auto Scaling groups that run in a VPC, specifies whether to assign a
-- public IP address to the group\'s instances.
awsAutoScalingLaunchConfigurationDetails_associatePublicIpAddress :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe Prelude.Bool)
awsAutoScalingLaunchConfigurationDetails_associatePublicIpAddress = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {associatePublicIpAddress = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | The security groups to assign to the instances in the Auto Scaling
-- group.
awsAutoScalingLaunchConfigurationDetails_securityGroups :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe [Prelude.Text])
awsAutoScalingLaunchConfigurationDetails_securityGroups = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {securityGroups} -> securityGroups) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {securityGroups = a} :: AwsAutoScalingLaunchConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

-- | The maximum hourly price to be paid for any Spot Instance that is
-- launched to fulfill the request.
awsAutoScalingLaunchConfigurationDetails_spotPrice :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationDetails_spotPrice = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {spotPrice} -> spotPrice) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {spotPrice = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | The creation date and time for the launch configuration.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsAutoScalingLaunchConfigurationDetails_createdTime :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationDetails_createdTime = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {createdTime} -> createdTime) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {createdTime = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | Indicates the type of monitoring for instances in the group.
awsAutoScalingLaunchConfigurationDetails_instanceMonitoring :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe AwsAutoScalingLaunchConfigurationInstanceMonitoringDetails)
awsAutoScalingLaunchConfigurationDetails_instanceMonitoring = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {instanceMonitoring} -> instanceMonitoring) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {instanceMonitoring = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | The name of the key pair.
awsAutoScalingLaunchConfigurationDetails_keyName :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationDetails_keyName = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {keyName} -> keyName) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {keyName = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | The identifiers of one or more security groups for the VPC that is
-- specified in @ClassicLinkVPCId@.
awsAutoScalingLaunchConfigurationDetails_classicLinkVpcSecurityGroups :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe [Prelude.Text])
awsAutoScalingLaunchConfigurationDetails_classicLinkVpcSecurityGroups = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {classicLinkVpcSecurityGroups} -> classicLinkVpcSecurityGroups) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {classicLinkVpcSecurityGroups = a} :: AwsAutoScalingLaunchConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the RAM disk associated with the AMI.
awsAutoScalingLaunchConfigurationDetails_ramdiskId :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationDetails_ramdiskId = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {ramdiskId} -> ramdiskId) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {ramdiskId = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | The identifier of the kernel associated with the AMI.
awsAutoScalingLaunchConfigurationDetails_kernelId :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationDetails_kernelId = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {kernelId} -> kernelId) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {kernelId = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | The instance type for the instances.
awsAutoScalingLaunchConfigurationDetails_instanceType :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationDetails_instanceType = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {instanceType} -> instanceType) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {instanceType = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | Whether the launch configuration is optimized for Amazon EBS I\/O.
awsAutoScalingLaunchConfigurationDetails_ebsOptimized :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe Prelude.Bool)
awsAutoScalingLaunchConfigurationDetails_ebsOptimized = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {ebsOptimized} -> ebsOptimized) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {ebsOptimized = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | The user data to make available to the launched EC2 instances. Must be
-- base64-encoded text.
awsAutoScalingLaunchConfigurationDetails_userData :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationDetails_userData = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {userData} -> userData) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {userData = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | The identifier of a ClassicLink-enabled VPC that EC2-Classic instances
-- are linked to.
awsAutoScalingLaunchConfigurationDetails_classicLinkVpcId :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationDetails_classicLinkVpcId = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {classicLinkVpcId} -> classicLinkVpcId) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {classicLinkVpcId = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | The name or the ARN of the instance profile associated with the IAM role
-- for the instance. The instance profile contains the IAM role.
awsAutoScalingLaunchConfigurationDetails_iamInstanceProfile :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationDetails_iamInstanceProfile = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {iamInstanceProfile} -> iamInstanceProfile) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {iamInstanceProfile = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | The identifier of the Amazon Machine Image (AMI) that is used to launch
-- EC2 instances.
awsAutoScalingLaunchConfigurationDetails_imageId :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationDetails_imageId = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {imageId} -> imageId) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {imageId = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | The name of the launch configuration.
awsAutoScalingLaunchConfigurationDetails_launchConfigurationName :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationDetails_launchConfigurationName = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {launchConfigurationName} -> launchConfigurationName) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {launchConfigurationName = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | The tenancy of the instance. An instance with @dedicated@ tenancy runs
-- on isolated, single-tenant hardware and can only be launched into a VPC.
awsAutoScalingLaunchConfigurationDetails_placementTenancy :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingLaunchConfigurationDetails_placementTenancy = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {placementTenancy} -> placementTenancy) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {placementTenancy = a} :: AwsAutoScalingLaunchConfigurationDetails)

-- | Specifies the block devices for the instance.
awsAutoScalingLaunchConfigurationDetails_blockDeviceMappings :: Lens.Lens' AwsAutoScalingLaunchConfigurationDetails (Prelude.Maybe [AwsAutoScalingLaunchConfigurationBlockDeviceMappingsDetails])
awsAutoScalingLaunchConfigurationDetails_blockDeviceMappings = Lens.lens (\AwsAutoScalingLaunchConfigurationDetails' {blockDeviceMappings} -> blockDeviceMappings) (\s@AwsAutoScalingLaunchConfigurationDetails' {} a -> s {blockDeviceMappings = a} :: AwsAutoScalingLaunchConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    AwsAutoScalingLaunchConfigurationDetails
  where
  parseJSON =
    Core.withObject
      "AwsAutoScalingLaunchConfigurationDetails"
      ( \x ->
          AwsAutoScalingLaunchConfigurationDetails'
            Prelude.<$> (x Core..:? "AssociatePublicIpAddress")
            Prelude.<*> (x Core..:? "SecurityGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "SpotPrice")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "InstanceMonitoring")
            Prelude.<*> (x Core..:? "KeyName")
            Prelude.<*> ( x Core..:? "ClassicLinkVpcSecurityGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "RamdiskId")
            Prelude.<*> (x Core..:? "KernelId")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "EbsOptimized")
            Prelude.<*> (x Core..:? "UserData")
            Prelude.<*> (x Core..:? "ClassicLinkVpcId")
            Prelude.<*> (x Core..:? "IamInstanceProfile")
            Prelude.<*> (x Core..:? "ImageId")
            Prelude.<*> (x Core..:? "LaunchConfigurationName")
            Prelude.<*> (x Core..:? "PlacementTenancy")
            Prelude.<*> ( x Core..:? "BlockDeviceMappings"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AwsAutoScalingLaunchConfigurationDetails
  where
  hashWithSalt
    salt'
    AwsAutoScalingLaunchConfigurationDetails' {..} =
      salt' `Prelude.hashWithSalt` blockDeviceMappings
        `Prelude.hashWithSalt` placementTenancy
        `Prelude.hashWithSalt` launchConfigurationName
        `Prelude.hashWithSalt` imageId
        `Prelude.hashWithSalt` iamInstanceProfile
        `Prelude.hashWithSalt` classicLinkVpcId
        `Prelude.hashWithSalt` userData
        `Prelude.hashWithSalt` ebsOptimized
        `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` kernelId
        `Prelude.hashWithSalt` ramdiskId
        `Prelude.hashWithSalt` classicLinkVpcSecurityGroups
        `Prelude.hashWithSalt` keyName
        `Prelude.hashWithSalt` instanceMonitoring
        `Prelude.hashWithSalt` createdTime
        `Prelude.hashWithSalt` spotPrice
        `Prelude.hashWithSalt` securityGroups
        `Prelude.hashWithSalt` associatePublicIpAddress

instance
  Prelude.NFData
    AwsAutoScalingLaunchConfigurationDetails
  where
  rnf AwsAutoScalingLaunchConfigurationDetails' {..} =
    Prelude.rnf associatePublicIpAddress
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf placementTenancy
      `Prelude.seq` Prelude.rnf launchConfigurationName
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf iamInstanceProfile
      `Prelude.seq` Prelude.rnf classicLinkVpcId
      `Prelude.seq` Prelude.rnf userData
      `Prelude.seq` Prelude.rnf ebsOptimized
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf ramdiskId
      `Prelude.seq` Prelude.rnf classicLinkVpcSecurityGroups
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf instanceMonitoring
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf spotPrice
      `Prelude.seq` Prelude.rnf securityGroups

instance
  Core.ToJSON
    AwsAutoScalingLaunchConfigurationDetails
  where
  toJSON AwsAutoScalingLaunchConfigurationDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AssociatePublicIpAddress" Core..=)
              Prelude.<$> associatePublicIpAddress,
            ("SecurityGroups" Core..=)
              Prelude.<$> securityGroups,
            ("SpotPrice" Core..=) Prelude.<$> spotPrice,
            ("CreatedTime" Core..=) Prelude.<$> createdTime,
            ("InstanceMonitoring" Core..=)
              Prelude.<$> instanceMonitoring,
            ("KeyName" Core..=) Prelude.<$> keyName,
            ("ClassicLinkVpcSecurityGroups" Core..=)
              Prelude.<$> classicLinkVpcSecurityGroups,
            ("RamdiskId" Core..=) Prelude.<$> ramdiskId,
            ("KernelId" Core..=) Prelude.<$> kernelId,
            ("InstanceType" Core..=) Prelude.<$> instanceType,
            ("EbsOptimized" Core..=) Prelude.<$> ebsOptimized,
            ("UserData" Core..=) Prelude.<$> userData,
            ("ClassicLinkVpcId" Core..=)
              Prelude.<$> classicLinkVpcId,
            ("IamInstanceProfile" Core..=)
              Prelude.<$> iamInstanceProfile,
            ("ImageId" Core..=) Prelude.<$> imageId,
            ("LaunchConfigurationName" Core..=)
              Prelude.<$> launchConfigurationName,
            ("PlacementTenancy" Core..=)
              Prelude.<$> placementTenancy,
            ("BlockDeviceMappings" Core..=)
              Prelude.<$> blockDeviceMappings
          ]
      )
