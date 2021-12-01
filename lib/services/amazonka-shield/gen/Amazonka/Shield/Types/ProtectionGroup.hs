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
-- Module      : Amazonka.Shield.Types.ProtectionGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.ProtectionGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Shield.Types.ProtectedResourceType
import Amazonka.Shield.Types.ProtectionGroupAggregation
import Amazonka.Shield.Types.ProtectionGroupPattern

-- | A grouping of protected resources that you and Shield Advanced can
-- monitor as a collective. This resource grouping improves the accuracy of
-- detection and reduces false positives.
--
-- /See:/ 'newProtectionGroup' smart constructor.
data ProtectionGroup = ProtectionGroup'
  { -- | The resource type to include in the protection group. All protected
    -- resources of this type are included in the protection group. You must
    -- set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not
    -- set it for any other @Pattern@ setting.
    resourceType :: Prelude.Maybe ProtectedResourceType,
    -- | The ARN (Amazon Resource Name) of the protection group.
    protectionGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the protection group. You use this to identify the
    -- protection group in lists and to manage the protection group, for
    -- example to update, delete, or describe it.
    protectionGroupId :: Prelude.Text,
    -- | Defines how Shield combines resource data for the group in order to
    -- detect, mitigate, and report events.
    --
    -- -   Sum - Use the total traffic across the group. This is a good choice
    --     for most cases. Examples include Elastic IP addresses for EC2
    --     instances that scale manually or automatically.
    --
    -- -   Mean - Use the average of the traffic across the group. This is a
    --     good choice for resources that share traffic uniformly. Examples
    --     include accelerators and load balancers.
    --
    -- -   Max - Use the highest traffic from each resource. This is useful for
    --     resources that don\'t share traffic and for resources that share
    --     that traffic in a non-uniform way. Examples include Amazon
    --     CloudFront distributions and origin resources for CloudFront
    --     distributions.
    aggregation :: ProtectionGroupAggregation,
    -- | The criteria to use to choose the protected resources for inclusion in
    -- the group. You can include all resources that have protections, provide
    -- a list of resource Amazon Resource Names (ARNs), or include all
    -- resources of a specified resource type.
    pattern' :: ProtectionGroupPattern,
    -- | The Amazon Resource Names (ARNs) of the resources to include in the
    -- protection group. You must set this when you set @Pattern@ to
    -- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
    members :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'protectionGroup_resourceType' - The resource type to include in the protection group. All protected
-- resources of this type are included in the protection group. You must
-- set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not
-- set it for any other @Pattern@ setting.
--
-- 'protectionGroupArn', 'protectionGroup_protectionGroupArn' - The ARN (Amazon Resource Name) of the protection group.
--
-- 'protectionGroupId', 'protectionGroup_protectionGroupId' - The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
--
-- 'aggregation', 'protectionGroup_aggregation' - Defines how Shield combines resource data for the group in order to
-- detect, mitigate, and report events.
--
-- -   Sum - Use the total traffic across the group. This is a good choice
--     for most cases. Examples include Elastic IP addresses for EC2
--     instances that scale manually or automatically.
--
-- -   Mean - Use the average of the traffic across the group. This is a
--     good choice for resources that share traffic uniformly. Examples
--     include accelerators and load balancers.
--
-- -   Max - Use the highest traffic from each resource. This is useful for
--     resources that don\'t share traffic and for resources that share
--     that traffic in a non-uniform way. Examples include Amazon
--     CloudFront distributions and origin resources for CloudFront
--     distributions.
--
-- 'pattern'', 'protectionGroup_pattern' - The criteria to use to choose the protected resources for inclusion in
-- the group. You can include all resources that have protections, provide
-- a list of resource Amazon Resource Names (ARNs), or include all
-- resources of a specified resource type.
--
-- 'members', 'protectionGroup_members' - The Amazon Resource Names (ARNs) of the resources to include in the
-- protection group. You must set this when you set @Pattern@ to
-- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
newProtectionGroup ::
  -- | 'protectionGroupId'
  Prelude.Text ->
  -- | 'aggregation'
  ProtectionGroupAggregation ->
  -- | 'pattern''
  ProtectionGroupPattern ->
  ProtectionGroup
newProtectionGroup
  pProtectionGroupId_
  pAggregation_
  pPattern_ =
    ProtectionGroup'
      { resourceType = Prelude.Nothing,
        protectionGroupArn = Prelude.Nothing,
        protectionGroupId = pProtectionGroupId_,
        aggregation = pAggregation_,
        pattern' = pPattern_,
        members = Prelude.mempty
      }

-- | The resource type to include in the protection group. All protected
-- resources of this type are included in the protection group. You must
-- set this when you set @Pattern@ to @BY_RESOURCE_TYPE@ and you must not
-- set it for any other @Pattern@ setting.
protectionGroup_resourceType :: Lens.Lens' ProtectionGroup (Prelude.Maybe ProtectedResourceType)
protectionGroup_resourceType = Lens.lens (\ProtectionGroup' {resourceType} -> resourceType) (\s@ProtectionGroup' {} a -> s {resourceType = a} :: ProtectionGroup)

-- | The ARN (Amazon Resource Name) of the protection group.
protectionGroup_protectionGroupArn :: Lens.Lens' ProtectionGroup (Prelude.Maybe Prelude.Text)
protectionGroup_protectionGroupArn = Lens.lens (\ProtectionGroup' {protectionGroupArn} -> protectionGroupArn) (\s@ProtectionGroup' {} a -> s {protectionGroupArn = a} :: ProtectionGroup)

-- | The name of the protection group. You use this to identify the
-- protection group in lists and to manage the protection group, for
-- example to update, delete, or describe it.
protectionGroup_protectionGroupId :: Lens.Lens' ProtectionGroup Prelude.Text
protectionGroup_protectionGroupId = Lens.lens (\ProtectionGroup' {protectionGroupId} -> protectionGroupId) (\s@ProtectionGroup' {} a -> s {protectionGroupId = a} :: ProtectionGroup)

-- | Defines how Shield combines resource data for the group in order to
-- detect, mitigate, and report events.
--
-- -   Sum - Use the total traffic across the group. This is a good choice
--     for most cases. Examples include Elastic IP addresses for EC2
--     instances that scale manually or automatically.
--
-- -   Mean - Use the average of the traffic across the group. This is a
--     good choice for resources that share traffic uniformly. Examples
--     include accelerators and load balancers.
--
-- -   Max - Use the highest traffic from each resource. This is useful for
--     resources that don\'t share traffic and for resources that share
--     that traffic in a non-uniform way. Examples include Amazon
--     CloudFront distributions and origin resources for CloudFront
--     distributions.
protectionGroup_aggregation :: Lens.Lens' ProtectionGroup ProtectionGroupAggregation
protectionGroup_aggregation = Lens.lens (\ProtectionGroup' {aggregation} -> aggregation) (\s@ProtectionGroup' {} a -> s {aggregation = a} :: ProtectionGroup)

-- | The criteria to use to choose the protected resources for inclusion in
-- the group. You can include all resources that have protections, provide
-- a list of resource Amazon Resource Names (ARNs), or include all
-- resources of a specified resource type.
protectionGroup_pattern :: Lens.Lens' ProtectionGroup ProtectionGroupPattern
protectionGroup_pattern = Lens.lens (\ProtectionGroup' {pattern'} -> pattern') (\s@ProtectionGroup' {} a -> s {pattern' = a} :: ProtectionGroup)

-- | The Amazon Resource Names (ARNs) of the resources to include in the
-- protection group. You must set this when you set @Pattern@ to
-- @ARBITRARY@ and you must not set it for any other @Pattern@ setting.
protectionGroup_members :: Lens.Lens' ProtectionGroup [Prelude.Text]
protectionGroup_members = Lens.lens (\ProtectionGroup' {members} -> members) (\s@ProtectionGroup' {} a -> s {members = a} :: ProtectionGroup) Prelude.. Lens.coerced

instance Core.FromJSON ProtectionGroup where
  parseJSON =
    Core.withObject
      "ProtectionGroup"
      ( \x ->
          ProtectionGroup'
            Prelude.<$> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "ProtectionGroupArn")
            Prelude.<*> (x Core..: "ProtectionGroupId")
            Prelude.<*> (x Core..: "Aggregation")
            Prelude.<*> (x Core..: "Pattern")
            Prelude.<*> (x Core..:? "Members" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ProtectionGroup where
  hashWithSalt salt' ProtectionGroup' {..} =
    salt' `Prelude.hashWithSalt` members
      `Prelude.hashWithSalt` pattern'
      `Prelude.hashWithSalt` aggregation
      `Prelude.hashWithSalt` protectionGroupId
      `Prelude.hashWithSalt` protectionGroupArn
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ProtectionGroup where
  rnf ProtectionGroup' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf members
      `Prelude.seq` Prelude.rnf pattern'
      `Prelude.seq` Prelude.rnf aggregation
      `Prelude.seq` Prelude.rnf protectionGroupId
      `Prelude.seq` Prelude.rnf protectionGroupArn
