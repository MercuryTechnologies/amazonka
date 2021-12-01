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
-- Module      : Amazonka.CognitoIdentityProvider.Types.AdminCreateUserConfigType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.AdminCreateUserConfigType where

import Amazonka.CognitoIdentityProvider.Types.MessageTemplateType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration for creating a new user profile.
--
-- /See:/ 'newAdminCreateUserConfigType' smart constructor.
data AdminCreateUserConfigType = AdminCreateUserConfigType'
  { -- | Set to @True@ if only the administrator is allowed to create user
    -- profiles. Set to @False@ if users can sign themselves up via an app.
    allowAdminCreateUserOnly :: Prelude.Maybe Prelude.Bool,
    -- | The user account expiration limit, in days, after which the account is
    -- no longer usable. To reset the account after that time limit, you must
    -- call @AdminCreateUser@ again, specifying @\"RESEND\"@ for the
    -- @MessageAction@ parameter. The default value for this parameter is 7.
    --
    -- If you set a value for @TemporaryPasswordValidityDays@ in
    -- @PasswordPolicy@, that value will be used and
    -- @UnusedAccountValidityDays@ will be deprecated for that user pool.
    unusedAccountValidityDays :: Prelude.Maybe Prelude.Natural,
    -- | The message template to be used for the welcome message to new users.
    --
    -- See also
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-message-customizations.html#cognito-user-pool-settings-user-invitation-message-customization Customizing User Invitation Messages>.
    inviteMessageTemplate :: Prelude.Maybe MessageTemplateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdminCreateUserConfigType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowAdminCreateUserOnly', 'adminCreateUserConfigType_allowAdminCreateUserOnly' - Set to @True@ if only the administrator is allowed to create user
-- profiles. Set to @False@ if users can sign themselves up via an app.
--
-- 'unusedAccountValidityDays', 'adminCreateUserConfigType_unusedAccountValidityDays' - The user account expiration limit, in days, after which the account is
-- no longer usable. To reset the account after that time limit, you must
-- call @AdminCreateUser@ again, specifying @\"RESEND\"@ for the
-- @MessageAction@ parameter. The default value for this parameter is 7.
--
-- If you set a value for @TemporaryPasswordValidityDays@ in
-- @PasswordPolicy@, that value will be used and
-- @UnusedAccountValidityDays@ will be deprecated for that user pool.
--
-- 'inviteMessageTemplate', 'adminCreateUserConfigType_inviteMessageTemplate' - The message template to be used for the welcome message to new users.
--
-- See also
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-message-customizations.html#cognito-user-pool-settings-user-invitation-message-customization Customizing User Invitation Messages>.
newAdminCreateUserConfigType ::
  AdminCreateUserConfigType
newAdminCreateUserConfigType =
  AdminCreateUserConfigType'
    { allowAdminCreateUserOnly =
        Prelude.Nothing,
      unusedAccountValidityDays = Prelude.Nothing,
      inviteMessageTemplate = Prelude.Nothing
    }

-- | Set to @True@ if only the administrator is allowed to create user
-- profiles. Set to @False@ if users can sign themselves up via an app.
adminCreateUserConfigType_allowAdminCreateUserOnly :: Lens.Lens' AdminCreateUserConfigType (Prelude.Maybe Prelude.Bool)
adminCreateUserConfigType_allowAdminCreateUserOnly = Lens.lens (\AdminCreateUserConfigType' {allowAdminCreateUserOnly} -> allowAdminCreateUserOnly) (\s@AdminCreateUserConfigType' {} a -> s {allowAdminCreateUserOnly = a} :: AdminCreateUserConfigType)

-- | The user account expiration limit, in days, after which the account is
-- no longer usable. To reset the account after that time limit, you must
-- call @AdminCreateUser@ again, specifying @\"RESEND\"@ for the
-- @MessageAction@ parameter. The default value for this parameter is 7.
--
-- If you set a value for @TemporaryPasswordValidityDays@ in
-- @PasswordPolicy@, that value will be used and
-- @UnusedAccountValidityDays@ will be deprecated for that user pool.
adminCreateUserConfigType_unusedAccountValidityDays :: Lens.Lens' AdminCreateUserConfigType (Prelude.Maybe Prelude.Natural)
adminCreateUserConfigType_unusedAccountValidityDays = Lens.lens (\AdminCreateUserConfigType' {unusedAccountValidityDays} -> unusedAccountValidityDays) (\s@AdminCreateUserConfigType' {} a -> s {unusedAccountValidityDays = a} :: AdminCreateUserConfigType)

-- | The message template to be used for the welcome message to new users.
--
-- See also
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-message-customizations.html#cognito-user-pool-settings-user-invitation-message-customization Customizing User Invitation Messages>.
adminCreateUserConfigType_inviteMessageTemplate :: Lens.Lens' AdminCreateUserConfigType (Prelude.Maybe MessageTemplateType)
adminCreateUserConfigType_inviteMessageTemplate = Lens.lens (\AdminCreateUserConfigType' {inviteMessageTemplate} -> inviteMessageTemplate) (\s@AdminCreateUserConfigType' {} a -> s {inviteMessageTemplate = a} :: AdminCreateUserConfigType)

instance Core.FromJSON AdminCreateUserConfigType where
  parseJSON =
    Core.withObject
      "AdminCreateUserConfigType"
      ( \x ->
          AdminCreateUserConfigType'
            Prelude.<$> (x Core..:? "AllowAdminCreateUserOnly")
            Prelude.<*> (x Core..:? "UnusedAccountValidityDays")
            Prelude.<*> (x Core..:? "InviteMessageTemplate")
      )

instance Prelude.Hashable AdminCreateUserConfigType where
  hashWithSalt salt' AdminCreateUserConfigType' {..} =
    salt' `Prelude.hashWithSalt` inviteMessageTemplate
      `Prelude.hashWithSalt` unusedAccountValidityDays
      `Prelude.hashWithSalt` allowAdminCreateUserOnly

instance Prelude.NFData AdminCreateUserConfigType where
  rnf AdminCreateUserConfigType' {..} =
    Prelude.rnf allowAdminCreateUserOnly
      `Prelude.seq` Prelude.rnf inviteMessageTemplate
      `Prelude.seq` Prelude.rnf unusedAccountValidityDays

instance Core.ToJSON AdminCreateUserConfigType where
  toJSON AdminCreateUserConfigType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AllowAdminCreateUserOnly" Core..=)
              Prelude.<$> allowAdminCreateUserOnly,
            ("UnusedAccountValidityDays" Core..=)
              Prelude.<$> unusedAccountValidityDays,
            ("InviteMessageTemplate" Core..=)
              Prelude.<$> inviteMessageTemplate
          ]
      )
