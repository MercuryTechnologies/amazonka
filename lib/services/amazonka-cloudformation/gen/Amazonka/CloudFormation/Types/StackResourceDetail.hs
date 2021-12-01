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
-- Module      : Amazonka.CloudFormation.Types.StackResourceDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackResourceDetail where

import Amazonka.CloudFormation.Types.ModuleInfo
import Amazonka.CloudFormation.Types.ResourceStatus
import Amazonka.CloudFormation.Types.StackResourceDriftInformation
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about the specified stack resource.
--
-- /See:/ 'newStackResourceDetail' smart constructor.
data StackResourceDetail = StackResourceDetail'
  { -- | The name or unique identifier that corresponds to a physical instance ID
    -- of a resource supported by CloudFormation.
    physicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | Success\/failure message associated with the resource.
    resourceStatusReason :: Prelude.Maybe Prelude.Text,
    -- | Information about whether the resource\'s actual configuration differs,
    -- or has /drifted/, from its expected configuration, as defined in the
    -- stack template and any values specified as template parameters. For more
    -- information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
    driftInformation :: Prelude.Maybe StackResourceDriftInformation,
    -- | Contains information about the module from which the resource was
    -- created, if the resource was created from a module included in the stack
    -- template.
    moduleInfo :: Prelude.Maybe ModuleInfo,
    -- | The content of the @Metadata@ attribute declared for the resource. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute>
    -- in the CloudFormation User Guide.
    metadata :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier of the stack.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | User defined description associated with the resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name associated with the stack.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | The logical name of the resource specified in the template.
    logicalResourceId :: Prelude.Text,
    -- | Type of resource. ((For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>
    -- in the CloudFormation User Guide.)
    resourceType :: Prelude.Text,
    -- | Time the status was updated.
    lastUpdatedTimestamp :: Core.ISO8601,
    -- | Current status of the resource.
    resourceStatus :: ResourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackResourceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'physicalResourceId', 'stackResourceDetail_physicalResourceId' - The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by CloudFormation.
--
-- 'resourceStatusReason', 'stackResourceDetail_resourceStatusReason' - Success\/failure message associated with the resource.
--
-- 'driftInformation', 'stackResourceDetail_driftInformation' - Information about whether the resource\'s actual configuration differs,
-- or has /drifted/, from its expected configuration, as defined in the
-- stack template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- 'moduleInfo', 'stackResourceDetail_moduleInfo' - Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
--
-- 'metadata', 'stackResourceDetail_metadata' - The content of the @Metadata@ attribute declared for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute>
-- in the CloudFormation User Guide.
--
-- 'stackId', 'stackResourceDetail_stackId' - Unique identifier of the stack.
--
-- 'description', 'stackResourceDetail_description' - User defined description associated with the resource.
--
-- 'stackName', 'stackResourceDetail_stackName' - The name associated with the stack.
--
-- 'logicalResourceId', 'stackResourceDetail_logicalResourceId' - The logical name of the resource specified in the template.
--
-- 'resourceType', 'stackResourceDetail_resourceType' - Type of resource. ((For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>
-- in the CloudFormation User Guide.)
--
-- 'lastUpdatedTimestamp', 'stackResourceDetail_lastUpdatedTimestamp' - Time the status was updated.
--
-- 'resourceStatus', 'stackResourceDetail_resourceStatus' - Current status of the resource.
newStackResourceDetail ::
  -- | 'logicalResourceId'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'lastUpdatedTimestamp'
  Prelude.UTCTime ->
  -- | 'resourceStatus'
  ResourceStatus ->
  StackResourceDetail
newStackResourceDetail
  pLogicalResourceId_
  pResourceType_
  pLastUpdatedTimestamp_
  pResourceStatus_ =
    StackResourceDetail'
      { physicalResourceId =
          Prelude.Nothing,
        resourceStatusReason = Prelude.Nothing,
        driftInformation = Prelude.Nothing,
        moduleInfo = Prelude.Nothing,
        metadata = Prelude.Nothing,
        stackId = Prelude.Nothing,
        description = Prelude.Nothing,
        stackName = Prelude.Nothing,
        logicalResourceId = pLogicalResourceId_,
        resourceType = pResourceType_,
        lastUpdatedTimestamp =
          Core._Time Lens.# pLastUpdatedTimestamp_,
        resourceStatus = pResourceStatus_
      }

-- | The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by CloudFormation.
stackResourceDetail_physicalResourceId :: Lens.Lens' StackResourceDetail (Prelude.Maybe Prelude.Text)
stackResourceDetail_physicalResourceId = Lens.lens (\StackResourceDetail' {physicalResourceId} -> physicalResourceId) (\s@StackResourceDetail' {} a -> s {physicalResourceId = a} :: StackResourceDetail)

-- | Success\/failure message associated with the resource.
stackResourceDetail_resourceStatusReason :: Lens.Lens' StackResourceDetail (Prelude.Maybe Prelude.Text)
stackResourceDetail_resourceStatusReason = Lens.lens (\StackResourceDetail' {resourceStatusReason} -> resourceStatusReason) (\s@StackResourceDetail' {} a -> s {resourceStatusReason = a} :: StackResourceDetail)

-- | Information about whether the resource\'s actual configuration differs,
-- or has /drifted/, from its expected configuration, as defined in the
-- stack template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
stackResourceDetail_driftInformation :: Lens.Lens' StackResourceDetail (Prelude.Maybe StackResourceDriftInformation)
stackResourceDetail_driftInformation = Lens.lens (\StackResourceDetail' {driftInformation} -> driftInformation) (\s@StackResourceDetail' {} a -> s {driftInformation = a} :: StackResourceDetail)

-- | Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
stackResourceDetail_moduleInfo :: Lens.Lens' StackResourceDetail (Prelude.Maybe ModuleInfo)
stackResourceDetail_moduleInfo = Lens.lens (\StackResourceDetail' {moduleInfo} -> moduleInfo) (\s@StackResourceDetail' {} a -> s {moduleInfo = a} :: StackResourceDetail)

-- | The content of the @Metadata@ attribute declared for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-attribute-metadata.html Metadata Attribute>
-- in the CloudFormation User Guide.
stackResourceDetail_metadata :: Lens.Lens' StackResourceDetail (Prelude.Maybe Prelude.Text)
stackResourceDetail_metadata = Lens.lens (\StackResourceDetail' {metadata} -> metadata) (\s@StackResourceDetail' {} a -> s {metadata = a} :: StackResourceDetail)

-- | Unique identifier of the stack.
stackResourceDetail_stackId :: Lens.Lens' StackResourceDetail (Prelude.Maybe Prelude.Text)
stackResourceDetail_stackId = Lens.lens (\StackResourceDetail' {stackId} -> stackId) (\s@StackResourceDetail' {} a -> s {stackId = a} :: StackResourceDetail)

-- | User defined description associated with the resource.
stackResourceDetail_description :: Lens.Lens' StackResourceDetail (Prelude.Maybe Prelude.Text)
stackResourceDetail_description = Lens.lens (\StackResourceDetail' {description} -> description) (\s@StackResourceDetail' {} a -> s {description = a} :: StackResourceDetail)

-- | The name associated with the stack.
stackResourceDetail_stackName :: Lens.Lens' StackResourceDetail (Prelude.Maybe Prelude.Text)
stackResourceDetail_stackName = Lens.lens (\StackResourceDetail' {stackName} -> stackName) (\s@StackResourceDetail' {} a -> s {stackName = a} :: StackResourceDetail)

-- | The logical name of the resource specified in the template.
stackResourceDetail_logicalResourceId :: Lens.Lens' StackResourceDetail Prelude.Text
stackResourceDetail_logicalResourceId = Lens.lens (\StackResourceDetail' {logicalResourceId} -> logicalResourceId) (\s@StackResourceDetail' {} a -> s {logicalResourceId = a} :: StackResourceDetail)

-- | Type of resource. ((For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>
-- in the CloudFormation User Guide.)
stackResourceDetail_resourceType :: Lens.Lens' StackResourceDetail Prelude.Text
stackResourceDetail_resourceType = Lens.lens (\StackResourceDetail' {resourceType} -> resourceType) (\s@StackResourceDetail' {} a -> s {resourceType = a} :: StackResourceDetail)

-- | Time the status was updated.
stackResourceDetail_lastUpdatedTimestamp :: Lens.Lens' StackResourceDetail Prelude.UTCTime
stackResourceDetail_lastUpdatedTimestamp = Lens.lens (\StackResourceDetail' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@StackResourceDetail' {} a -> s {lastUpdatedTimestamp = a} :: StackResourceDetail) Prelude.. Core._Time

-- | Current status of the resource.
stackResourceDetail_resourceStatus :: Lens.Lens' StackResourceDetail ResourceStatus
stackResourceDetail_resourceStatus = Lens.lens (\StackResourceDetail' {resourceStatus} -> resourceStatus) (\s@StackResourceDetail' {} a -> s {resourceStatus = a} :: StackResourceDetail)

instance Core.FromXML StackResourceDetail where
  parseXML x =
    StackResourceDetail'
      Prelude.<$> (x Core..@? "PhysicalResourceId")
      Prelude.<*> (x Core..@? "ResourceStatusReason")
      Prelude.<*> (x Core..@? "DriftInformation")
      Prelude.<*> (x Core..@? "ModuleInfo")
      Prelude.<*> (x Core..@? "Metadata")
      Prelude.<*> (x Core..@? "StackId")
      Prelude.<*> (x Core..@? "Description")
      Prelude.<*> (x Core..@? "StackName")
      Prelude.<*> (x Core..@ "LogicalResourceId")
      Prelude.<*> (x Core..@ "ResourceType")
      Prelude.<*> (x Core..@ "LastUpdatedTimestamp")
      Prelude.<*> (x Core..@ "ResourceStatus")

instance Prelude.Hashable StackResourceDetail where
  hashWithSalt salt' StackResourceDetail' {..} =
    salt' `Prelude.hashWithSalt` resourceStatus
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` moduleInfo
      `Prelude.hashWithSalt` driftInformation
      `Prelude.hashWithSalt` resourceStatusReason
      `Prelude.hashWithSalt` physicalResourceId

instance Prelude.NFData StackResourceDetail where
  rnf StackResourceDetail' {..} =
    Prelude.rnf physicalResourceId
      `Prelude.seq` Prelude.rnf resourceStatus
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf moduleInfo
      `Prelude.seq` Prelude.rnf driftInformation
      `Prelude.seq` Prelude.rnf resourceStatusReason
