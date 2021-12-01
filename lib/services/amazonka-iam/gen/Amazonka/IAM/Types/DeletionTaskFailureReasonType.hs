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
-- Module      : Amazonka.IAM.Types.DeletionTaskFailureReasonType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.DeletionTaskFailureReasonType where

import qualified Amazonka.Core as Core
import Amazonka.IAM.Types.RoleUsageType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The reason that the service-linked role deletion failed.
--
-- This data type is used as a response element in the
-- GetServiceLinkedRoleDeletionStatus operation.
--
-- /See:/ 'newDeletionTaskFailureReasonType' smart constructor.
data DeletionTaskFailureReasonType = DeletionTaskFailureReasonType'
  { -- | A list of objects that contains details about the service-linked role
    -- deletion failure, if that information is returned by the service. If the
    -- service-linked role has active sessions or if any resources that were
    -- used by the role have not been deleted from the linked service, the role
    -- can\'t be deleted. This parameter includes a list of the resources that
    -- are associated with the role and the Region in which the resources are
    -- being used.
    roleUsageList :: Prelude.Maybe [RoleUsageType],
    -- | A short description of the reason that the service-linked role deletion
    -- failed.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletionTaskFailureReasonType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleUsageList', 'deletionTaskFailureReasonType_roleUsageList' - A list of objects that contains details about the service-linked role
-- deletion failure, if that information is returned by the service. If the
-- service-linked role has active sessions or if any resources that were
-- used by the role have not been deleted from the linked service, the role
-- can\'t be deleted. This parameter includes a list of the resources that
-- are associated with the role and the Region in which the resources are
-- being used.
--
-- 'reason', 'deletionTaskFailureReasonType_reason' - A short description of the reason that the service-linked role deletion
-- failed.
newDeletionTaskFailureReasonType ::
  DeletionTaskFailureReasonType
newDeletionTaskFailureReasonType =
  DeletionTaskFailureReasonType'
    { roleUsageList =
        Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | A list of objects that contains details about the service-linked role
-- deletion failure, if that information is returned by the service. If the
-- service-linked role has active sessions or if any resources that were
-- used by the role have not been deleted from the linked service, the role
-- can\'t be deleted. This parameter includes a list of the resources that
-- are associated with the role and the Region in which the resources are
-- being used.
deletionTaskFailureReasonType_roleUsageList :: Lens.Lens' DeletionTaskFailureReasonType (Prelude.Maybe [RoleUsageType])
deletionTaskFailureReasonType_roleUsageList = Lens.lens (\DeletionTaskFailureReasonType' {roleUsageList} -> roleUsageList) (\s@DeletionTaskFailureReasonType' {} a -> s {roleUsageList = a} :: DeletionTaskFailureReasonType) Prelude.. Lens.mapping Lens.coerced

-- | A short description of the reason that the service-linked role deletion
-- failed.
deletionTaskFailureReasonType_reason :: Lens.Lens' DeletionTaskFailureReasonType (Prelude.Maybe Prelude.Text)
deletionTaskFailureReasonType_reason = Lens.lens (\DeletionTaskFailureReasonType' {reason} -> reason) (\s@DeletionTaskFailureReasonType' {} a -> s {reason = a} :: DeletionTaskFailureReasonType)

instance Core.FromXML DeletionTaskFailureReasonType where
  parseXML x =
    DeletionTaskFailureReasonType'
      Prelude.<$> ( x Core..@? "RoleUsageList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Reason")

instance
  Prelude.Hashable
    DeletionTaskFailureReasonType
  where
  hashWithSalt salt' DeletionTaskFailureReasonType' {..} =
    salt' `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` roleUsageList

instance Prelude.NFData DeletionTaskFailureReasonType where
  rnf DeletionTaskFailureReasonType' {..} =
    Prelude.rnf roleUsageList
      `Prelude.seq` Prelude.rnf reason
