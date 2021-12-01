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
-- Module      : Amazonka.Detective.Types.Account
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Types.Account where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An AWS account that is the administrator account of or a member of a
-- behavior graph.
--
-- /See:/ 'newAccount' smart constructor.
data Account = Account'
  { -- | The account identifier of the AWS account.
    accountId :: Prelude.Text,
    -- | The AWS account root user email address for the AWS account.
    emailAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Account' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'account_accountId' - The account identifier of the AWS account.
--
-- 'emailAddress', 'account_emailAddress' - The AWS account root user email address for the AWS account.
newAccount ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'emailAddress'
  Prelude.Text ->
  Account
newAccount pAccountId_ pEmailAddress_ =
  Account'
    { accountId = pAccountId_,
      emailAddress = pEmailAddress_
    }

-- | The account identifier of the AWS account.
account_accountId :: Lens.Lens' Account Prelude.Text
account_accountId = Lens.lens (\Account' {accountId} -> accountId) (\s@Account' {} a -> s {accountId = a} :: Account)

-- | The AWS account root user email address for the AWS account.
account_emailAddress :: Lens.Lens' Account Prelude.Text
account_emailAddress = Lens.lens (\Account' {emailAddress} -> emailAddress) (\s@Account' {} a -> s {emailAddress = a} :: Account)

instance Prelude.Hashable Account where
  hashWithSalt salt' Account' {..} =
    salt' `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData Account where
  rnf Account' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf emailAddress

instance Core.ToJSON Account where
  toJSON Account' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Core..= accountId),
            Prelude.Just ("EmailAddress" Core..= emailAddress)
          ]
      )
