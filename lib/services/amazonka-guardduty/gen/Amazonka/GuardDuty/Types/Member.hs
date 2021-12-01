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
-- Module      : Amazonka.GuardDuty.Types.Member
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Member where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the member account.
--
-- /See:/ 'newMember' smart constructor.
data Member = Member'
  { -- | The timestamp when the invitation was sent.
    invitedAt :: Prelude.Maybe Prelude.Text,
    -- | The detector ID of the member account.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the member account.
    accountId :: Prelude.Text,
    -- | The administrator account ID.
    masterId :: Prelude.Text,
    -- | The email address of the member account.
    email :: Prelude.Text,
    -- | The status of the relationship between the member and the administrator.
    relationshipStatus :: Prelude.Text,
    -- | The last-updated timestamp of the member.
    updatedAt :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Member' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invitedAt', 'member_invitedAt' - The timestamp when the invitation was sent.
--
-- 'detectorId', 'member_detectorId' - The detector ID of the member account.
--
-- 'accountId', 'member_accountId' - The ID of the member account.
--
-- 'masterId', 'member_masterId' - The administrator account ID.
--
-- 'email', 'member_email' - The email address of the member account.
--
-- 'relationshipStatus', 'member_relationshipStatus' - The status of the relationship between the member and the administrator.
--
-- 'updatedAt', 'member_updatedAt' - The last-updated timestamp of the member.
newMember ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'masterId'
  Prelude.Text ->
  -- | 'email'
  Prelude.Text ->
  -- | 'relationshipStatus'
  Prelude.Text ->
  -- | 'updatedAt'
  Prelude.Text ->
  Member
newMember
  pAccountId_
  pMasterId_
  pEmail_
  pRelationshipStatus_
  pUpdatedAt_ =
    Member'
      { invitedAt = Prelude.Nothing,
        detectorId = Prelude.Nothing,
        accountId = pAccountId_,
        masterId = pMasterId_,
        email = pEmail_,
        relationshipStatus = pRelationshipStatus_,
        updatedAt = pUpdatedAt_
      }

-- | The timestamp when the invitation was sent.
member_invitedAt :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_invitedAt = Lens.lens (\Member' {invitedAt} -> invitedAt) (\s@Member' {} a -> s {invitedAt = a} :: Member)

-- | The detector ID of the member account.
member_detectorId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_detectorId = Lens.lens (\Member' {detectorId} -> detectorId) (\s@Member' {} a -> s {detectorId = a} :: Member)

-- | The ID of the member account.
member_accountId :: Lens.Lens' Member Prelude.Text
member_accountId = Lens.lens (\Member' {accountId} -> accountId) (\s@Member' {} a -> s {accountId = a} :: Member)

-- | The administrator account ID.
member_masterId :: Lens.Lens' Member Prelude.Text
member_masterId = Lens.lens (\Member' {masterId} -> masterId) (\s@Member' {} a -> s {masterId = a} :: Member)

-- | The email address of the member account.
member_email :: Lens.Lens' Member Prelude.Text
member_email = Lens.lens (\Member' {email} -> email) (\s@Member' {} a -> s {email = a} :: Member)

-- | The status of the relationship between the member and the administrator.
member_relationshipStatus :: Lens.Lens' Member Prelude.Text
member_relationshipStatus = Lens.lens (\Member' {relationshipStatus} -> relationshipStatus) (\s@Member' {} a -> s {relationshipStatus = a} :: Member)

-- | The last-updated timestamp of the member.
member_updatedAt :: Lens.Lens' Member Prelude.Text
member_updatedAt = Lens.lens (\Member' {updatedAt} -> updatedAt) (\s@Member' {} a -> s {updatedAt = a} :: Member)

instance Core.FromJSON Member where
  parseJSON =
    Core.withObject
      "Member"
      ( \x ->
          Member'
            Prelude.<$> (x Core..:? "invitedAt")
            Prelude.<*> (x Core..:? "detectorId")
            Prelude.<*> (x Core..: "accountId")
            Prelude.<*> (x Core..: "masterId")
            Prelude.<*> (x Core..: "email")
            Prelude.<*> (x Core..: "relationshipStatus")
            Prelude.<*> (x Core..: "updatedAt")
      )

instance Prelude.Hashable Member where
  hashWithSalt salt' Member' {..} =
    salt' `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` relationshipStatus
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` masterId
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` invitedAt

instance Prelude.NFData Member where
  rnf Member' {..} =
    Prelude.rnf invitedAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf relationshipStatus
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf masterId
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf detectorId
