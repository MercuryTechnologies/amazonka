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
-- Module      : Amazonka.MacieV2.Types.IamUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.IamUser where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an Identity and Access Management (IAM) user
-- who performed an action on an affected resource.
--
-- /See:/ 'newIamUser' smart constructor.
data IamUser = IamUser'
  { -- | The unique identifier for the IAM user who performed the action.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the principal that performed the
    -- action. The last section of the ARN contains the name of the user who
    -- performed the action.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The user name of the IAM user who performed the action.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the Amazon Web Services account that\'s
    -- associated with the IAM user who performed the action.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IamUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalId', 'iamUser_principalId' - The unique identifier for the IAM user who performed the action.
--
-- 'arn', 'iamUser_arn' - The Amazon Resource Name (ARN) of the principal that performed the
-- action. The last section of the ARN contains the name of the user who
-- performed the action.
--
-- 'userName', 'iamUser_userName' - The user name of the IAM user who performed the action.
--
-- 'accountId', 'iamUser_accountId' - The unique identifier for the Amazon Web Services account that\'s
-- associated with the IAM user who performed the action.
newIamUser ::
  IamUser
newIamUser =
  IamUser'
    { principalId = Prelude.Nothing,
      arn = Prelude.Nothing,
      userName = Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | The unique identifier for the IAM user who performed the action.
iamUser_principalId :: Lens.Lens' IamUser (Prelude.Maybe Prelude.Text)
iamUser_principalId = Lens.lens (\IamUser' {principalId} -> principalId) (\s@IamUser' {} a -> s {principalId = a} :: IamUser)

-- | The Amazon Resource Name (ARN) of the principal that performed the
-- action. The last section of the ARN contains the name of the user who
-- performed the action.
iamUser_arn :: Lens.Lens' IamUser (Prelude.Maybe Prelude.Text)
iamUser_arn = Lens.lens (\IamUser' {arn} -> arn) (\s@IamUser' {} a -> s {arn = a} :: IamUser)

-- | The user name of the IAM user who performed the action.
iamUser_userName :: Lens.Lens' IamUser (Prelude.Maybe Prelude.Text)
iamUser_userName = Lens.lens (\IamUser' {userName} -> userName) (\s@IamUser' {} a -> s {userName = a} :: IamUser)

-- | The unique identifier for the Amazon Web Services account that\'s
-- associated with the IAM user who performed the action.
iamUser_accountId :: Lens.Lens' IamUser (Prelude.Maybe Prelude.Text)
iamUser_accountId = Lens.lens (\IamUser' {accountId} -> accountId) (\s@IamUser' {} a -> s {accountId = a} :: IamUser)

instance Core.FromJSON IamUser where
  parseJSON =
    Core.withObject
      "IamUser"
      ( \x ->
          IamUser'
            Prelude.<$> (x Core..:? "principalId")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "userName")
            Prelude.<*> (x Core..:? "accountId")
      )

instance Prelude.Hashable IamUser where
  hashWithSalt salt' IamUser' {..} =
    salt' `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` principalId

instance Prelude.NFData IamUser where
  rnf IamUser' {..} =
    Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf arn
