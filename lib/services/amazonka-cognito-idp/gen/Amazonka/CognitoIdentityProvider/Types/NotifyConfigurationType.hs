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
-- Module      : Amazonka.CognitoIdentityProvider.Types.NotifyConfigurationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.NotifyConfigurationType where

import Amazonka.CognitoIdentityProvider.Types.NotifyEmailType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The notify configuration type.
--
-- /See:/ 'newNotifyConfigurationType' smart constructor.
data NotifyConfigurationType = NotifyConfigurationType'
  { -- | The email template used when a detected risk event is allowed.
    noActionEmail :: Prelude.Maybe NotifyEmailType,
    -- | The email address that is sending the email. It must be either
    -- individually verified with Amazon SES, or from a domain that has been
    -- verified with Amazon SES.
    from :: Prelude.Maybe Prelude.Text,
    -- | The destination to which the receiver of an email should reply to.
    replyTo :: Prelude.Maybe Prelude.Text,
    -- | Email template used when a detected risk event is blocked.
    blockEmail :: Prelude.Maybe NotifyEmailType,
    -- | The MFA email template used when MFA is challenged as part of a detected
    -- risk.
    mfaEmail :: Prelude.Maybe NotifyEmailType,
    -- | The Amazon Resource Name (ARN) of the identity that is associated with
    -- the sending authorization policy. It permits Amazon Cognito to send for
    -- the email address specified in the @From@ parameter.
    sourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'noActionEmail', 'notifyConfigurationType_noActionEmail' - The email template used when a detected risk event is allowed.
--
-- 'from', 'notifyConfigurationType_from' - The email address that is sending the email. It must be either
-- individually verified with Amazon SES, or from a domain that has been
-- verified with Amazon SES.
--
-- 'replyTo', 'notifyConfigurationType_replyTo' - The destination to which the receiver of an email should reply to.
--
-- 'blockEmail', 'notifyConfigurationType_blockEmail' - Email template used when a detected risk event is blocked.
--
-- 'mfaEmail', 'notifyConfigurationType_mfaEmail' - The MFA email template used when MFA is challenged as part of a detected
-- risk.
--
-- 'sourceArn', 'notifyConfigurationType_sourceArn' - The Amazon Resource Name (ARN) of the identity that is associated with
-- the sending authorization policy. It permits Amazon Cognito to send for
-- the email address specified in the @From@ parameter.
newNotifyConfigurationType ::
  -- | 'sourceArn'
  Prelude.Text ->
  NotifyConfigurationType
newNotifyConfigurationType pSourceArn_ =
  NotifyConfigurationType'
    { noActionEmail =
        Prelude.Nothing,
      from = Prelude.Nothing,
      replyTo = Prelude.Nothing,
      blockEmail = Prelude.Nothing,
      mfaEmail = Prelude.Nothing,
      sourceArn = pSourceArn_
    }

-- | The email template used when a detected risk event is allowed.
notifyConfigurationType_noActionEmail :: Lens.Lens' NotifyConfigurationType (Prelude.Maybe NotifyEmailType)
notifyConfigurationType_noActionEmail = Lens.lens (\NotifyConfigurationType' {noActionEmail} -> noActionEmail) (\s@NotifyConfigurationType' {} a -> s {noActionEmail = a} :: NotifyConfigurationType)

-- | The email address that is sending the email. It must be either
-- individually verified with Amazon SES, or from a domain that has been
-- verified with Amazon SES.
notifyConfigurationType_from :: Lens.Lens' NotifyConfigurationType (Prelude.Maybe Prelude.Text)
notifyConfigurationType_from = Lens.lens (\NotifyConfigurationType' {from} -> from) (\s@NotifyConfigurationType' {} a -> s {from = a} :: NotifyConfigurationType)

-- | The destination to which the receiver of an email should reply to.
notifyConfigurationType_replyTo :: Lens.Lens' NotifyConfigurationType (Prelude.Maybe Prelude.Text)
notifyConfigurationType_replyTo = Lens.lens (\NotifyConfigurationType' {replyTo} -> replyTo) (\s@NotifyConfigurationType' {} a -> s {replyTo = a} :: NotifyConfigurationType)

-- | Email template used when a detected risk event is blocked.
notifyConfigurationType_blockEmail :: Lens.Lens' NotifyConfigurationType (Prelude.Maybe NotifyEmailType)
notifyConfigurationType_blockEmail = Lens.lens (\NotifyConfigurationType' {blockEmail} -> blockEmail) (\s@NotifyConfigurationType' {} a -> s {blockEmail = a} :: NotifyConfigurationType)

-- | The MFA email template used when MFA is challenged as part of a detected
-- risk.
notifyConfigurationType_mfaEmail :: Lens.Lens' NotifyConfigurationType (Prelude.Maybe NotifyEmailType)
notifyConfigurationType_mfaEmail = Lens.lens (\NotifyConfigurationType' {mfaEmail} -> mfaEmail) (\s@NotifyConfigurationType' {} a -> s {mfaEmail = a} :: NotifyConfigurationType)

-- | The Amazon Resource Name (ARN) of the identity that is associated with
-- the sending authorization policy. It permits Amazon Cognito to send for
-- the email address specified in the @From@ parameter.
notifyConfigurationType_sourceArn :: Lens.Lens' NotifyConfigurationType Prelude.Text
notifyConfigurationType_sourceArn = Lens.lens (\NotifyConfigurationType' {sourceArn} -> sourceArn) (\s@NotifyConfigurationType' {} a -> s {sourceArn = a} :: NotifyConfigurationType)

instance Core.FromJSON NotifyConfigurationType where
  parseJSON =
    Core.withObject
      "NotifyConfigurationType"
      ( \x ->
          NotifyConfigurationType'
            Prelude.<$> (x Core..:? "NoActionEmail")
            Prelude.<*> (x Core..:? "From")
            Prelude.<*> (x Core..:? "ReplyTo")
            Prelude.<*> (x Core..:? "BlockEmail")
            Prelude.<*> (x Core..:? "MfaEmail")
            Prelude.<*> (x Core..: "SourceArn")
      )

instance Prelude.Hashable NotifyConfigurationType where
  hashWithSalt salt' NotifyConfigurationType' {..} =
    salt' `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` mfaEmail
      `Prelude.hashWithSalt` blockEmail
      `Prelude.hashWithSalt` replyTo
      `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` noActionEmail

instance Prelude.NFData NotifyConfigurationType where
  rnf NotifyConfigurationType' {..} =
    Prelude.rnf noActionEmail
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf mfaEmail
      `Prelude.seq` Prelude.rnf blockEmail
      `Prelude.seq` Prelude.rnf replyTo
      `Prelude.seq` Prelude.rnf from

instance Core.ToJSON NotifyConfigurationType where
  toJSON NotifyConfigurationType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NoActionEmail" Core..=) Prelude.<$> noActionEmail,
            ("From" Core..=) Prelude.<$> from,
            ("ReplyTo" Core..=) Prelude.<$> replyTo,
            ("BlockEmail" Core..=) Prelude.<$> blockEmail,
            ("MfaEmail" Core..=) Prelude.<$> mfaEmail,
            Prelude.Just ("SourceArn" Core..= sourceArn)
          ]
      )
