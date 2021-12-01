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
-- Module      : Amazonka.IAM.Types.PolicyUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.PolicyUser where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a user that a managed policy is attached to.
--
-- This data type is used as a response element in the
-- ListEntitiesForPolicy operation.
--
-- For more information about managed policies, refer to
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- /See:/ 'newPolicyUser' smart constructor.
data PolicyUser = PolicyUser'
  { -- | The name (friendly name, not ARN) identifying the user.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The stable and unique string identifying the user. For more information
    -- about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    userId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'policyUser_userName' - The name (friendly name, not ARN) identifying the user.
--
-- 'userId', 'policyUser_userId' - The stable and unique string identifying the user. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
newPolicyUser ::
  PolicyUser
newPolicyUser =
  PolicyUser'
    { userName = Prelude.Nothing,
      userId = Prelude.Nothing
    }

-- | The name (friendly name, not ARN) identifying the user.
policyUser_userName :: Lens.Lens' PolicyUser (Prelude.Maybe Prelude.Text)
policyUser_userName = Lens.lens (\PolicyUser' {userName} -> userName) (\s@PolicyUser' {} a -> s {userName = a} :: PolicyUser)

-- | The stable and unique string identifying the user. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
policyUser_userId :: Lens.Lens' PolicyUser (Prelude.Maybe Prelude.Text)
policyUser_userId = Lens.lens (\PolicyUser' {userId} -> userId) (\s@PolicyUser' {} a -> s {userId = a} :: PolicyUser)

instance Core.FromXML PolicyUser where
  parseXML x =
    PolicyUser'
      Prelude.<$> (x Core..@? "UserName")
      Prelude.<*> (x Core..@? "UserId")

instance Prelude.Hashable PolicyUser where
  hashWithSalt salt' PolicyUser' {..} =
    salt' `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` userName

instance Prelude.NFData PolicyUser where
  rnf PolicyUser' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf userId
