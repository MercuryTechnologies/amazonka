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
-- Module      : Amazonka.QuickSight.Types.AccountSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AccountSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Edition

-- | The Amazon QuickSight settings associated with your Amazon Web Services
-- account.
--
-- /See:/ 'newAccountSettings' smart constructor.
data AccountSettings = AccountSettings'
  { -- | The edition of Amazon QuickSight that you\'re currently subscribed to:
    -- Enterprise edition or Standard edition.
    edition :: Prelude.Maybe Edition,
    -- | The \"account name\" you provided for the Amazon QuickSight subscription
    -- in your Amazon Web Services account. You create this name when you sign
    -- up for Amazon QuickSight. It is unique in all of Amazon Web Services and
    -- it appears only when users sign in.
    accountName :: Prelude.Maybe Prelude.Text,
    -- | The default Amazon QuickSight namespace for your Amazon Web Services
    -- account.
    defaultNamespace :: Prelude.Maybe Prelude.Text,
    -- | The main notification email for your Amazon QuickSight subscription.
    notificationEmail :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edition', 'accountSettings_edition' - The edition of Amazon QuickSight that you\'re currently subscribed to:
-- Enterprise edition or Standard edition.
--
-- 'accountName', 'accountSettings_accountName' - The \"account name\" you provided for the Amazon QuickSight subscription
-- in your Amazon Web Services account. You create this name when you sign
-- up for Amazon QuickSight. It is unique in all of Amazon Web Services and
-- it appears only when users sign in.
--
-- 'defaultNamespace', 'accountSettings_defaultNamespace' - The default Amazon QuickSight namespace for your Amazon Web Services
-- account.
--
-- 'notificationEmail', 'accountSettings_notificationEmail' - The main notification email for your Amazon QuickSight subscription.
newAccountSettings ::
  AccountSettings
newAccountSettings =
  AccountSettings'
    { edition = Prelude.Nothing,
      accountName = Prelude.Nothing,
      defaultNamespace = Prelude.Nothing,
      notificationEmail = Prelude.Nothing
    }

-- | The edition of Amazon QuickSight that you\'re currently subscribed to:
-- Enterprise edition or Standard edition.
accountSettings_edition :: Lens.Lens' AccountSettings (Prelude.Maybe Edition)
accountSettings_edition = Lens.lens (\AccountSettings' {edition} -> edition) (\s@AccountSettings' {} a -> s {edition = a} :: AccountSettings)

-- | The \"account name\" you provided for the Amazon QuickSight subscription
-- in your Amazon Web Services account. You create this name when you sign
-- up for Amazon QuickSight. It is unique in all of Amazon Web Services and
-- it appears only when users sign in.
accountSettings_accountName :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Text)
accountSettings_accountName = Lens.lens (\AccountSettings' {accountName} -> accountName) (\s@AccountSettings' {} a -> s {accountName = a} :: AccountSettings)

-- | The default Amazon QuickSight namespace for your Amazon Web Services
-- account.
accountSettings_defaultNamespace :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Text)
accountSettings_defaultNamespace = Lens.lens (\AccountSettings' {defaultNamespace} -> defaultNamespace) (\s@AccountSettings' {} a -> s {defaultNamespace = a} :: AccountSettings)

-- | The main notification email for your Amazon QuickSight subscription.
accountSettings_notificationEmail :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Text)
accountSettings_notificationEmail = Lens.lens (\AccountSettings' {notificationEmail} -> notificationEmail) (\s@AccountSettings' {} a -> s {notificationEmail = a} :: AccountSettings)

instance Core.FromJSON AccountSettings where
  parseJSON =
    Core.withObject
      "AccountSettings"
      ( \x ->
          AccountSettings'
            Prelude.<$> (x Core..:? "Edition")
            Prelude.<*> (x Core..:? "AccountName")
            Prelude.<*> (x Core..:? "DefaultNamespace")
            Prelude.<*> (x Core..:? "NotificationEmail")
      )

instance Prelude.Hashable AccountSettings where
  hashWithSalt salt' AccountSettings' {..} =
    salt' `Prelude.hashWithSalt` notificationEmail
      `Prelude.hashWithSalt` defaultNamespace
      `Prelude.hashWithSalt` accountName
      `Prelude.hashWithSalt` edition

instance Prelude.NFData AccountSettings where
  rnf AccountSettings' {..} =
    Prelude.rnf edition
      `Prelude.seq` Prelude.rnf notificationEmail
      `Prelude.seq` Prelude.rnf defaultNamespace
      `Prelude.seq` Prelude.rnf accountName
