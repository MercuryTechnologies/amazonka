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
-- Module      : Amazonka.Chime.Types.Account
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Account where

import Amazonka.Chime.Types.AccountStatus
import Amazonka.Chime.Types.AccountType
import Amazonka.Chime.Types.License
import Amazonka.Chime.Types.SigninDelegateGroup
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Chime account details. An AWS account can have multiple
-- Amazon Chime accounts.
--
-- /See:/ 'newAccount' smart constructor.
data Account = Account'
  { -- | The sign-in delegate groups associated with the account.
    signinDelegateGroups :: Prelude.Maybe [SigninDelegateGroup],
    -- | The status of the account.
    accountStatus :: Prelude.Maybe AccountStatus,
    -- | The default license for the Amazon Chime account.
    defaultLicense :: Prelude.Maybe License,
    -- | Supported licenses for the Amazon Chime account.
    supportedLicenses :: Prelude.Maybe [License],
    -- | The Amazon Chime account creation timestamp, in ISO 8601 format.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Chime account type. For more information about different
    -- account types, see
    -- <https://docs.aws.amazon.com/chime/latest/ag/manage-chime-account.html Managing Your Amazon Chime Accounts>
    -- in the /Amazon Chime Administration Guide/.
    accountType :: Prelude.Maybe AccountType,
    -- | The AWS account ID.
    awsAccountId :: Prelude.Text,
    -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The Amazon Chime account name.
    name :: Prelude.Text
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
-- 'signinDelegateGroups', 'account_signinDelegateGroups' - The sign-in delegate groups associated with the account.
--
-- 'accountStatus', 'account_accountStatus' - The status of the account.
--
-- 'defaultLicense', 'account_defaultLicense' - The default license for the Amazon Chime account.
--
-- 'supportedLicenses', 'account_supportedLicenses' - Supported licenses for the Amazon Chime account.
--
-- 'createdTimestamp', 'account_createdTimestamp' - The Amazon Chime account creation timestamp, in ISO 8601 format.
--
-- 'accountType', 'account_accountType' - The Amazon Chime account type. For more information about different
-- account types, see
-- <https://docs.aws.amazon.com/chime/latest/ag/manage-chime-account.html Managing Your Amazon Chime Accounts>
-- in the /Amazon Chime Administration Guide/.
--
-- 'awsAccountId', 'account_awsAccountId' - The AWS account ID.
--
-- 'accountId', 'account_accountId' - The Amazon Chime account ID.
--
-- 'name', 'account_name' - The Amazon Chime account name.
newAccount ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'accountId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  Account
newAccount pAwsAccountId_ pAccountId_ pName_ =
  Account'
    { signinDelegateGroups = Prelude.Nothing,
      accountStatus = Prelude.Nothing,
      defaultLicense = Prelude.Nothing,
      supportedLicenses = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      accountType = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      accountId = pAccountId_,
      name = pName_
    }

-- | The sign-in delegate groups associated with the account.
account_signinDelegateGroups :: Lens.Lens' Account (Prelude.Maybe [SigninDelegateGroup])
account_signinDelegateGroups = Lens.lens (\Account' {signinDelegateGroups} -> signinDelegateGroups) (\s@Account' {} a -> s {signinDelegateGroups = a} :: Account) Prelude.. Lens.mapping Lens.coerced

-- | The status of the account.
account_accountStatus :: Lens.Lens' Account (Prelude.Maybe AccountStatus)
account_accountStatus = Lens.lens (\Account' {accountStatus} -> accountStatus) (\s@Account' {} a -> s {accountStatus = a} :: Account)

-- | The default license for the Amazon Chime account.
account_defaultLicense :: Lens.Lens' Account (Prelude.Maybe License)
account_defaultLicense = Lens.lens (\Account' {defaultLicense} -> defaultLicense) (\s@Account' {} a -> s {defaultLicense = a} :: Account)

-- | Supported licenses for the Amazon Chime account.
account_supportedLicenses :: Lens.Lens' Account (Prelude.Maybe [License])
account_supportedLicenses = Lens.lens (\Account' {supportedLicenses} -> supportedLicenses) (\s@Account' {} a -> s {supportedLicenses = a} :: Account) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Chime account creation timestamp, in ISO 8601 format.
account_createdTimestamp :: Lens.Lens' Account (Prelude.Maybe Prelude.UTCTime)
account_createdTimestamp = Lens.lens (\Account' {createdTimestamp} -> createdTimestamp) (\s@Account' {} a -> s {createdTimestamp = a} :: Account) Prelude.. Lens.mapping Core._Time

-- | The Amazon Chime account type. For more information about different
-- account types, see
-- <https://docs.aws.amazon.com/chime/latest/ag/manage-chime-account.html Managing Your Amazon Chime Accounts>
-- in the /Amazon Chime Administration Guide/.
account_accountType :: Lens.Lens' Account (Prelude.Maybe AccountType)
account_accountType = Lens.lens (\Account' {accountType} -> accountType) (\s@Account' {} a -> s {accountType = a} :: Account)

-- | The AWS account ID.
account_awsAccountId :: Lens.Lens' Account Prelude.Text
account_awsAccountId = Lens.lens (\Account' {awsAccountId} -> awsAccountId) (\s@Account' {} a -> s {awsAccountId = a} :: Account)

-- | The Amazon Chime account ID.
account_accountId :: Lens.Lens' Account Prelude.Text
account_accountId = Lens.lens (\Account' {accountId} -> accountId) (\s@Account' {} a -> s {accountId = a} :: Account)

-- | The Amazon Chime account name.
account_name :: Lens.Lens' Account Prelude.Text
account_name = Lens.lens (\Account' {name} -> name) (\s@Account' {} a -> s {name = a} :: Account)

instance Core.FromJSON Account where
  parseJSON =
    Core.withObject
      "Account"
      ( \x ->
          Account'
            Prelude.<$> ( x Core..:? "SigninDelegateGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "AccountStatus")
            Prelude.<*> (x Core..:? "DefaultLicense")
            Prelude.<*> ( x Core..:? "SupportedLicenses"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "AccountType")
            Prelude.<*> (x Core..: "AwsAccountId")
            Prelude.<*> (x Core..: "AccountId")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable Account where
  hashWithSalt salt' Account' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` accountType
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` supportedLicenses
      `Prelude.hashWithSalt` defaultLicense
      `Prelude.hashWithSalt` accountStatus
      `Prelude.hashWithSalt` signinDelegateGroups

instance Prelude.NFData Account where
  rnf Account' {..} =
    Prelude.rnf signinDelegateGroups
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf accountType
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf supportedLicenses
      `Prelude.seq` Prelude.rnf defaultLicense
      `Prelude.seq` Prelude.rnf accountStatus
