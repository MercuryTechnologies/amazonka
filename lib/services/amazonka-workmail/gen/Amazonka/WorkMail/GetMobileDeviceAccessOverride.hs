{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WorkMail.GetMobileDeviceAccessOverride
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the mobile device access override for the given WorkMail
-- organization, user, and device.
module Amazonka.WorkMail.GetMobileDeviceAccessOverride
  ( -- * Creating a Request
    GetMobileDeviceAccessOverride (..),
    newGetMobileDeviceAccessOverride,

    -- * Request Lenses
    getMobileDeviceAccessOverride_organizationId,
    getMobileDeviceAccessOverride_userId,
    getMobileDeviceAccessOverride_deviceId,

    -- * Destructuring the Response
    GetMobileDeviceAccessOverrideResponse (..),
    newGetMobileDeviceAccessOverrideResponse,

    -- * Response Lenses
    getMobileDeviceAccessOverrideResponse_effect,
    getMobileDeviceAccessOverrideResponse_userId,
    getMobileDeviceAccessOverrideResponse_dateCreated,
    getMobileDeviceAccessOverrideResponse_dateModified,
    getMobileDeviceAccessOverrideResponse_deviceId,
    getMobileDeviceAccessOverrideResponse_description,
    getMobileDeviceAccessOverrideResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newGetMobileDeviceAccessOverride' smart constructor.
data GetMobileDeviceAccessOverride = GetMobileDeviceAccessOverride'
  { -- | The Amazon WorkMail organization to which you want to apply the
    -- override.
    organizationId :: Prelude.Text,
    -- | Identifies the WorkMail user for the override. Accepts the following
    -- types of user identities:
    --
    -- -   User ID: @12345678-1234-1234-1234-123456789012@ or
    --     @S-1-1-12-1234567890-123456789-123456789-1234@
    --
    -- -   Email address: @user\@domain.tld@
    --
    -- -   User name: @user@
    userId :: Prelude.Text,
    -- | The mobile device to which the override applies. @DeviceId@ is case
    -- insensitive.
    deviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMobileDeviceAccessOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'getMobileDeviceAccessOverride_organizationId' - The Amazon WorkMail organization to which you want to apply the
-- override.
--
-- 'userId', 'getMobileDeviceAccessOverride_userId' - Identifies the WorkMail user for the override. Accepts the following
-- types of user identities:
--
-- -   User ID: @12345678-1234-1234-1234-123456789012@ or
--     @S-1-1-12-1234567890-123456789-123456789-1234@
--
-- -   Email address: @user\@domain.tld@
--
-- -   User name: @user@
--
-- 'deviceId', 'getMobileDeviceAccessOverride_deviceId' - The mobile device to which the override applies. @DeviceId@ is case
-- insensitive.
newGetMobileDeviceAccessOverride ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'deviceId'
  Prelude.Text ->
  GetMobileDeviceAccessOverride
newGetMobileDeviceAccessOverride
  pOrganizationId_
  pUserId_
  pDeviceId_ =
    GetMobileDeviceAccessOverride'
      { organizationId =
          pOrganizationId_,
        userId = pUserId_,
        deviceId = pDeviceId_
      }

-- | The Amazon WorkMail organization to which you want to apply the
-- override.
getMobileDeviceAccessOverride_organizationId :: Lens.Lens' GetMobileDeviceAccessOverride Prelude.Text
getMobileDeviceAccessOverride_organizationId = Lens.lens (\GetMobileDeviceAccessOverride' {organizationId} -> organizationId) (\s@GetMobileDeviceAccessOverride' {} a -> s {organizationId = a} :: GetMobileDeviceAccessOverride)

-- | Identifies the WorkMail user for the override. Accepts the following
-- types of user identities:
--
-- -   User ID: @12345678-1234-1234-1234-123456789012@ or
--     @S-1-1-12-1234567890-123456789-123456789-1234@
--
-- -   Email address: @user\@domain.tld@
--
-- -   User name: @user@
getMobileDeviceAccessOverride_userId :: Lens.Lens' GetMobileDeviceAccessOverride Prelude.Text
getMobileDeviceAccessOverride_userId = Lens.lens (\GetMobileDeviceAccessOverride' {userId} -> userId) (\s@GetMobileDeviceAccessOverride' {} a -> s {userId = a} :: GetMobileDeviceAccessOverride)

-- | The mobile device to which the override applies. @DeviceId@ is case
-- insensitive.
getMobileDeviceAccessOverride_deviceId :: Lens.Lens' GetMobileDeviceAccessOverride Prelude.Text
getMobileDeviceAccessOverride_deviceId = Lens.lens (\GetMobileDeviceAccessOverride' {deviceId} -> deviceId) (\s@GetMobileDeviceAccessOverride' {} a -> s {deviceId = a} :: GetMobileDeviceAccessOverride)

instance
  Core.AWSRequest
    GetMobileDeviceAccessOverride
  where
  type
    AWSResponse GetMobileDeviceAccessOverride =
      GetMobileDeviceAccessOverrideResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMobileDeviceAccessOverrideResponse'
            Prelude.<$> (x Core..?> "Effect")
            Prelude.<*> (x Core..?> "UserId")
            Prelude.<*> (x Core..?> "DateCreated")
            Prelude.<*> (x Core..?> "DateModified")
            Prelude.<*> (x Core..?> "DeviceId")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetMobileDeviceAccessOverride
  where
  hashWithSalt salt' GetMobileDeviceAccessOverride' {..} =
    salt' `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` organizationId

instance Prelude.NFData GetMobileDeviceAccessOverride where
  rnf GetMobileDeviceAccessOverride' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf userId

instance Core.ToHeaders GetMobileDeviceAccessOverride where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.GetMobileDeviceAccessOverride" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetMobileDeviceAccessOverride where
  toJSON GetMobileDeviceAccessOverride' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Core..= organizationId),
            Prelude.Just ("UserId" Core..= userId),
            Prelude.Just ("DeviceId" Core..= deviceId)
          ]
      )

instance Core.ToPath GetMobileDeviceAccessOverride where
  toPath = Prelude.const "/"

instance Core.ToQuery GetMobileDeviceAccessOverride where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMobileDeviceAccessOverrideResponse' smart constructor.
data GetMobileDeviceAccessOverrideResponse = GetMobileDeviceAccessOverrideResponse'
  { -- | The effect of the override, @ALLOW@ or @DENY@.
    effect :: Prelude.Maybe MobileDeviceAccessRuleEffect,
    -- | The WorkMail user to which the access override applies.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The date the override was first created.
    dateCreated :: Prelude.Maybe Core.POSIX,
    -- | The date the description was last modified.
    dateModified :: Prelude.Maybe Core.POSIX,
    -- | The device to which the access override applies.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | A description of the override.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMobileDeviceAccessOverrideResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'effect', 'getMobileDeviceAccessOverrideResponse_effect' - The effect of the override, @ALLOW@ or @DENY@.
--
-- 'userId', 'getMobileDeviceAccessOverrideResponse_userId' - The WorkMail user to which the access override applies.
--
-- 'dateCreated', 'getMobileDeviceAccessOverrideResponse_dateCreated' - The date the override was first created.
--
-- 'dateModified', 'getMobileDeviceAccessOverrideResponse_dateModified' - The date the description was last modified.
--
-- 'deviceId', 'getMobileDeviceAccessOverrideResponse_deviceId' - The device to which the access override applies.
--
-- 'description', 'getMobileDeviceAccessOverrideResponse_description' - A description of the override.
--
-- 'httpStatus', 'getMobileDeviceAccessOverrideResponse_httpStatus' - The response's http status code.
newGetMobileDeviceAccessOverrideResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMobileDeviceAccessOverrideResponse
newGetMobileDeviceAccessOverrideResponse pHttpStatus_ =
  GetMobileDeviceAccessOverrideResponse'
    { effect =
        Prelude.Nothing,
      userId = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      dateModified = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The effect of the override, @ALLOW@ or @DENY@.
getMobileDeviceAccessOverrideResponse_effect :: Lens.Lens' GetMobileDeviceAccessOverrideResponse (Prelude.Maybe MobileDeviceAccessRuleEffect)
getMobileDeviceAccessOverrideResponse_effect = Lens.lens (\GetMobileDeviceAccessOverrideResponse' {effect} -> effect) (\s@GetMobileDeviceAccessOverrideResponse' {} a -> s {effect = a} :: GetMobileDeviceAccessOverrideResponse)

-- | The WorkMail user to which the access override applies.
getMobileDeviceAccessOverrideResponse_userId :: Lens.Lens' GetMobileDeviceAccessOverrideResponse (Prelude.Maybe Prelude.Text)
getMobileDeviceAccessOverrideResponse_userId = Lens.lens (\GetMobileDeviceAccessOverrideResponse' {userId} -> userId) (\s@GetMobileDeviceAccessOverrideResponse' {} a -> s {userId = a} :: GetMobileDeviceAccessOverrideResponse)

-- | The date the override was first created.
getMobileDeviceAccessOverrideResponse_dateCreated :: Lens.Lens' GetMobileDeviceAccessOverrideResponse (Prelude.Maybe Prelude.UTCTime)
getMobileDeviceAccessOverrideResponse_dateCreated = Lens.lens (\GetMobileDeviceAccessOverrideResponse' {dateCreated} -> dateCreated) (\s@GetMobileDeviceAccessOverrideResponse' {} a -> s {dateCreated = a} :: GetMobileDeviceAccessOverrideResponse) Prelude.. Lens.mapping Core._Time

-- | The date the description was last modified.
getMobileDeviceAccessOverrideResponse_dateModified :: Lens.Lens' GetMobileDeviceAccessOverrideResponse (Prelude.Maybe Prelude.UTCTime)
getMobileDeviceAccessOverrideResponse_dateModified = Lens.lens (\GetMobileDeviceAccessOverrideResponse' {dateModified} -> dateModified) (\s@GetMobileDeviceAccessOverrideResponse' {} a -> s {dateModified = a} :: GetMobileDeviceAccessOverrideResponse) Prelude.. Lens.mapping Core._Time

-- | The device to which the access override applies.
getMobileDeviceAccessOverrideResponse_deviceId :: Lens.Lens' GetMobileDeviceAccessOverrideResponse (Prelude.Maybe Prelude.Text)
getMobileDeviceAccessOverrideResponse_deviceId = Lens.lens (\GetMobileDeviceAccessOverrideResponse' {deviceId} -> deviceId) (\s@GetMobileDeviceAccessOverrideResponse' {} a -> s {deviceId = a} :: GetMobileDeviceAccessOverrideResponse)

-- | A description of the override.
getMobileDeviceAccessOverrideResponse_description :: Lens.Lens' GetMobileDeviceAccessOverrideResponse (Prelude.Maybe Prelude.Text)
getMobileDeviceAccessOverrideResponse_description = Lens.lens (\GetMobileDeviceAccessOverrideResponse' {description} -> description) (\s@GetMobileDeviceAccessOverrideResponse' {} a -> s {description = a} :: GetMobileDeviceAccessOverrideResponse)

-- | The response's http status code.
getMobileDeviceAccessOverrideResponse_httpStatus :: Lens.Lens' GetMobileDeviceAccessOverrideResponse Prelude.Int
getMobileDeviceAccessOverrideResponse_httpStatus = Lens.lens (\GetMobileDeviceAccessOverrideResponse' {httpStatus} -> httpStatus) (\s@GetMobileDeviceAccessOverrideResponse' {} a -> s {httpStatus = a} :: GetMobileDeviceAccessOverrideResponse)

instance
  Prelude.NFData
    GetMobileDeviceAccessOverrideResponse
  where
  rnf GetMobileDeviceAccessOverrideResponse' {..} =
    Prelude.rnf effect
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf dateModified
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf userId
