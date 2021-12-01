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
-- Module      : Amazonka.CloudFormation.Types.StackResourceDrift
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackResourceDrift where

import Amazonka.CloudFormation.Types.ModuleInfo
import Amazonka.CloudFormation.Types.PhysicalResourceIdContextKeyValuePair
import Amazonka.CloudFormation.Types.PropertyDifference
import Amazonka.CloudFormation.Types.StackResourceDriftStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the drift information for a resource that has been checked for
-- drift. This includes actual and expected property values for resources
-- in which CloudFormation has detected drift. Only resource properties
-- explicitly defined in the stack template are checked for drift. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- Resources that do not currently support drift detection cannot be
-- checked. For a list of resources that support drift detection, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection>.
--
-- Use DetectStackResourceDrift to detect drift on individual resources, or
-- DetectStackDrift to detect drift on all resources in a given stack that
-- support drift detection.
--
-- /See:/ 'newStackResourceDrift' smart constructor.
data StackResourceDrift = StackResourceDrift'
  { -- | A JSON structure containing the actual property values of the stack
    -- resource.
    --
    -- For resources whose @StackResourceDriftStatus@ is @DELETED@, this
    -- structure will not be present.
    actualProperties :: Prelude.Maybe Prelude.Text,
    -- | The name or unique identifier that corresponds to a physical instance ID
    -- of a resource supported by CloudFormation.
    physicalResourceId :: Prelude.Maybe Prelude.Text,
    -- | Context information that enables CloudFormation to uniquely identify a
    -- resource. CloudFormation uses context key-value pairs in cases where a
    -- resource\'s logical and physical IDs are not enough to uniquely identify
    -- that resource. Each context key-value pair specifies a unique resource
    -- that contains the targeted resource.
    physicalResourceIdContext :: Prelude.Maybe [PhysicalResourceIdContextKeyValuePair],
    -- | A collection of the resource properties whose actual values differ from
    -- their expected values. These will be present only for resources whose
    -- @StackResourceDriftStatus@ is @MODIFIED@.
    propertyDifferences :: Prelude.Maybe [PropertyDifference],
    -- | Contains information about the module from which the resource was
    -- created, if the resource was created from a module included in the stack
    -- template.
    moduleInfo :: Prelude.Maybe ModuleInfo,
    -- | A JSON structure containing the expected property values of the stack
    -- resource, as defined in the stack template and any values specified as
    -- template parameters.
    --
    -- For resources whose @StackResourceDriftStatus@ is @DELETED@, this
    -- structure will not be present.
    expectedProperties :: Prelude.Maybe Prelude.Text,
    -- | The ID of the stack.
    stackId :: Prelude.Text,
    -- | The logical name of the resource specified in the template.
    logicalResourceId :: Prelude.Text,
    -- | The type of the resource.
    resourceType :: Prelude.Text,
    -- | Status of the resource\'s actual configuration compared to its expected
    -- configuration
    --
    -- -   @DELETED@: The resource differs from its expected template
    --     configuration because the resource has been deleted.
    --
    -- -   @MODIFIED@: One or more resource properties differ from their
    --     expected values (as defined in the stack template and any values
    --     specified as template parameters).
    --
    -- -   @IN_SYNC@: The resources\'s actual configuration matches its
    --     expected template configuration.
    --
    -- -   @NOT_CHECKED@: CloudFormation does not currently return this value.
    stackResourceDriftStatus :: StackResourceDriftStatus,
    -- | Time at which CloudFormation performed drift detection on the stack
    -- resource.
    timestamp :: Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackResourceDrift' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actualProperties', 'stackResourceDrift_actualProperties' - A JSON structure containing the actual property values of the stack
-- resource.
--
-- For resources whose @StackResourceDriftStatus@ is @DELETED@, this
-- structure will not be present.
--
-- 'physicalResourceId', 'stackResourceDrift_physicalResourceId' - The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by CloudFormation.
--
-- 'physicalResourceIdContext', 'stackResourceDrift_physicalResourceIdContext' - Context information that enables CloudFormation to uniquely identify a
-- resource. CloudFormation uses context key-value pairs in cases where a
-- resource\'s logical and physical IDs are not enough to uniquely identify
-- that resource. Each context key-value pair specifies a unique resource
-- that contains the targeted resource.
--
-- 'propertyDifferences', 'stackResourceDrift_propertyDifferences' - A collection of the resource properties whose actual values differ from
-- their expected values. These will be present only for resources whose
-- @StackResourceDriftStatus@ is @MODIFIED@.
--
-- 'moduleInfo', 'stackResourceDrift_moduleInfo' - Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
--
-- 'expectedProperties', 'stackResourceDrift_expectedProperties' - A JSON structure containing the expected property values of the stack
-- resource, as defined in the stack template and any values specified as
-- template parameters.
--
-- For resources whose @StackResourceDriftStatus@ is @DELETED@, this
-- structure will not be present.
--
-- 'stackId', 'stackResourceDrift_stackId' - The ID of the stack.
--
-- 'logicalResourceId', 'stackResourceDrift_logicalResourceId' - The logical name of the resource specified in the template.
--
-- 'resourceType', 'stackResourceDrift_resourceType' - The type of the resource.
--
-- 'stackResourceDriftStatus', 'stackResourceDrift_stackResourceDriftStatus' - Status of the resource\'s actual configuration compared to its expected
-- configuration
--
-- -   @DELETED@: The resource differs from its expected template
--     configuration because the resource has been deleted.
--
-- -   @MODIFIED@: One or more resource properties differ from their
--     expected values (as defined in the stack template and any values
--     specified as template parameters).
--
-- -   @IN_SYNC@: The resources\'s actual configuration matches its
--     expected template configuration.
--
-- -   @NOT_CHECKED@: CloudFormation does not currently return this value.
--
-- 'timestamp', 'stackResourceDrift_timestamp' - Time at which CloudFormation performed drift detection on the stack
-- resource.
newStackResourceDrift ::
  -- | 'stackId'
  Prelude.Text ->
  -- | 'logicalResourceId'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'stackResourceDriftStatus'
  StackResourceDriftStatus ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  StackResourceDrift
newStackResourceDrift
  pStackId_
  pLogicalResourceId_
  pResourceType_
  pStackResourceDriftStatus_
  pTimestamp_ =
    StackResourceDrift'
      { actualProperties =
          Prelude.Nothing,
        physicalResourceId = Prelude.Nothing,
        physicalResourceIdContext = Prelude.Nothing,
        propertyDifferences = Prelude.Nothing,
        moduleInfo = Prelude.Nothing,
        expectedProperties = Prelude.Nothing,
        stackId = pStackId_,
        logicalResourceId = pLogicalResourceId_,
        resourceType = pResourceType_,
        stackResourceDriftStatus =
          pStackResourceDriftStatus_,
        timestamp = Core._Time Lens.# pTimestamp_
      }

-- | A JSON structure containing the actual property values of the stack
-- resource.
--
-- For resources whose @StackResourceDriftStatus@ is @DELETED@, this
-- structure will not be present.
stackResourceDrift_actualProperties :: Lens.Lens' StackResourceDrift (Prelude.Maybe Prelude.Text)
stackResourceDrift_actualProperties = Lens.lens (\StackResourceDrift' {actualProperties} -> actualProperties) (\s@StackResourceDrift' {} a -> s {actualProperties = a} :: StackResourceDrift)

-- | The name or unique identifier that corresponds to a physical instance ID
-- of a resource supported by CloudFormation.
stackResourceDrift_physicalResourceId :: Lens.Lens' StackResourceDrift (Prelude.Maybe Prelude.Text)
stackResourceDrift_physicalResourceId = Lens.lens (\StackResourceDrift' {physicalResourceId} -> physicalResourceId) (\s@StackResourceDrift' {} a -> s {physicalResourceId = a} :: StackResourceDrift)

-- | Context information that enables CloudFormation to uniquely identify a
-- resource. CloudFormation uses context key-value pairs in cases where a
-- resource\'s logical and physical IDs are not enough to uniquely identify
-- that resource. Each context key-value pair specifies a unique resource
-- that contains the targeted resource.
stackResourceDrift_physicalResourceIdContext :: Lens.Lens' StackResourceDrift (Prelude.Maybe [PhysicalResourceIdContextKeyValuePair])
stackResourceDrift_physicalResourceIdContext = Lens.lens (\StackResourceDrift' {physicalResourceIdContext} -> physicalResourceIdContext) (\s@StackResourceDrift' {} a -> s {physicalResourceIdContext = a} :: StackResourceDrift) Prelude.. Lens.mapping Lens.coerced

-- | A collection of the resource properties whose actual values differ from
-- their expected values. These will be present only for resources whose
-- @StackResourceDriftStatus@ is @MODIFIED@.
stackResourceDrift_propertyDifferences :: Lens.Lens' StackResourceDrift (Prelude.Maybe [PropertyDifference])
stackResourceDrift_propertyDifferences = Lens.lens (\StackResourceDrift' {propertyDifferences} -> propertyDifferences) (\s@StackResourceDrift' {} a -> s {propertyDifferences = a} :: StackResourceDrift) Prelude.. Lens.mapping Lens.coerced

-- | Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
stackResourceDrift_moduleInfo :: Lens.Lens' StackResourceDrift (Prelude.Maybe ModuleInfo)
stackResourceDrift_moduleInfo = Lens.lens (\StackResourceDrift' {moduleInfo} -> moduleInfo) (\s@StackResourceDrift' {} a -> s {moduleInfo = a} :: StackResourceDrift)

-- | A JSON structure containing the expected property values of the stack
-- resource, as defined in the stack template and any values specified as
-- template parameters.
--
-- For resources whose @StackResourceDriftStatus@ is @DELETED@, this
-- structure will not be present.
stackResourceDrift_expectedProperties :: Lens.Lens' StackResourceDrift (Prelude.Maybe Prelude.Text)
stackResourceDrift_expectedProperties = Lens.lens (\StackResourceDrift' {expectedProperties} -> expectedProperties) (\s@StackResourceDrift' {} a -> s {expectedProperties = a} :: StackResourceDrift)

-- | The ID of the stack.
stackResourceDrift_stackId :: Lens.Lens' StackResourceDrift Prelude.Text
stackResourceDrift_stackId = Lens.lens (\StackResourceDrift' {stackId} -> stackId) (\s@StackResourceDrift' {} a -> s {stackId = a} :: StackResourceDrift)

-- | The logical name of the resource specified in the template.
stackResourceDrift_logicalResourceId :: Lens.Lens' StackResourceDrift Prelude.Text
stackResourceDrift_logicalResourceId = Lens.lens (\StackResourceDrift' {logicalResourceId} -> logicalResourceId) (\s@StackResourceDrift' {} a -> s {logicalResourceId = a} :: StackResourceDrift)

-- | The type of the resource.
stackResourceDrift_resourceType :: Lens.Lens' StackResourceDrift Prelude.Text
stackResourceDrift_resourceType = Lens.lens (\StackResourceDrift' {resourceType} -> resourceType) (\s@StackResourceDrift' {} a -> s {resourceType = a} :: StackResourceDrift)

-- | Status of the resource\'s actual configuration compared to its expected
-- configuration
--
-- -   @DELETED@: The resource differs from its expected template
--     configuration because the resource has been deleted.
--
-- -   @MODIFIED@: One or more resource properties differ from their
--     expected values (as defined in the stack template and any values
--     specified as template parameters).
--
-- -   @IN_SYNC@: The resources\'s actual configuration matches its
--     expected template configuration.
--
-- -   @NOT_CHECKED@: CloudFormation does not currently return this value.
stackResourceDrift_stackResourceDriftStatus :: Lens.Lens' StackResourceDrift StackResourceDriftStatus
stackResourceDrift_stackResourceDriftStatus = Lens.lens (\StackResourceDrift' {stackResourceDriftStatus} -> stackResourceDriftStatus) (\s@StackResourceDrift' {} a -> s {stackResourceDriftStatus = a} :: StackResourceDrift)

-- | Time at which CloudFormation performed drift detection on the stack
-- resource.
stackResourceDrift_timestamp :: Lens.Lens' StackResourceDrift Prelude.UTCTime
stackResourceDrift_timestamp = Lens.lens (\StackResourceDrift' {timestamp} -> timestamp) (\s@StackResourceDrift' {} a -> s {timestamp = a} :: StackResourceDrift) Prelude.. Core._Time

instance Core.FromXML StackResourceDrift where
  parseXML x =
    StackResourceDrift'
      Prelude.<$> (x Core..@? "ActualProperties")
      Prelude.<*> (x Core..@? "PhysicalResourceId")
      Prelude.<*> ( x Core..@? "PhysicalResourceIdContext"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "PropertyDifferences"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "ModuleInfo")
      Prelude.<*> (x Core..@? "ExpectedProperties")
      Prelude.<*> (x Core..@ "StackId")
      Prelude.<*> (x Core..@ "LogicalResourceId")
      Prelude.<*> (x Core..@ "ResourceType")
      Prelude.<*> (x Core..@ "StackResourceDriftStatus")
      Prelude.<*> (x Core..@ "Timestamp")

instance Prelude.Hashable StackResourceDrift where
  hashWithSalt salt' StackResourceDrift' {..} =
    salt' `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` stackResourceDriftStatus
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` logicalResourceId
      `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` expectedProperties
      `Prelude.hashWithSalt` moduleInfo
      `Prelude.hashWithSalt` propertyDifferences
      `Prelude.hashWithSalt` physicalResourceIdContext
      `Prelude.hashWithSalt` physicalResourceId
      `Prelude.hashWithSalt` actualProperties

instance Prelude.NFData StackResourceDrift where
  rnf StackResourceDrift' {..} =
    Prelude.rnf actualProperties
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf stackResourceDriftStatus
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf logicalResourceId
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf expectedProperties
      `Prelude.seq` Prelude.rnf moduleInfo
      `Prelude.seq` Prelude.rnf propertyDifferences
      `Prelude.seq` Prelude.rnf physicalResourceIdContext
      `Prelude.seq` Prelude.rnf physicalResourceId
