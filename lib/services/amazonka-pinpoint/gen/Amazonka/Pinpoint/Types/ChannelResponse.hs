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
-- Module      : Amazonka.Pinpoint.Types.ChannelResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ChannelResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the general settings and status of a channel
-- for an application.
--
-- /See:/ 'newChannelResponse' smart constructor.
data ChannelResponse = ChannelResponse'
  { -- | The date and time, in ISO 8601 format, when the channel was last
    -- modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the channel is enabled for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the channel is archived.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for the application.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The current version of the channel.
    version :: Prelude.Maybe Prelude.Int,
    -- | (Deprecated) An identifier for the channel. This property is retained
    -- only for backward compatibility.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in ISO 8601 format, when the channel was enabled.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The user who last modified the channel.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'channelResponse_lastModifiedDate' - The date and time, in ISO 8601 format, when the channel was last
-- modified.
--
-- 'enabled', 'channelResponse_enabled' - Specifies whether the channel is enabled for the application.
--
-- 'isArchived', 'channelResponse_isArchived' - Specifies whether the channel is archived.
--
-- 'applicationId', 'channelResponse_applicationId' - The unique identifier for the application.
--
-- 'version', 'channelResponse_version' - The current version of the channel.
--
-- 'id', 'channelResponse_id' - (Deprecated) An identifier for the channel. This property is retained
-- only for backward compatibility.
--
-- 'creationDate', 'channelResponse_creationDate' - The date and time, in ISO 8601 format, when the channel was enabled.
--
-- 'lastModifiedBy', 'channelResponse_lastModifiedBy' - The user who last modified the channel.
--
-- 'hasCredential', 'channelResponse_hasCredential' - (Not used) This property is retained only for backward compatibility.
newChannelResponse ::
  ChannelResponse
newChannelResponse =
  ChannelResponse'
    { lastModifiedDate =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      isArchived = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      version = Prelude.Nothing,
      id = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      hasCredential = Prelude.Nothing
    }

-- | The date and time, in ISO 8601 format, when the channel was last
-- modified.
channelResponse_lastModifiedDate :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Text)
channelResponse_lastModifiedDate = Lens.lens (\ChannelResponse' {lastModifiedDate} -> lastModifiedDate) (\s@ChannelResponse' {} a -> s {lastModifiedDate = a} :: ChannelResponse)

-- | Specifies whether the channel is enabled for the application.
channelResponse_enabled :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Bool)
channelResponse_enabled = Lens.lens (\ChannelResponse' {enabled} -> enabled) (\s@ChannelResponse' {} a -> s {enabled = a} :: ChannelResponse)

-- | Specifies whether the channel is archived.
channelResponse_isArchived :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Bool)
channelResponse_isArchived = Lens.lens (\ChannelResponse' {isArchived} -> isArchived) (\s@ChannelResponse' {} a -> s {isArchived = a} :: ChannelResponse)

-- | The unique identifier for the application.
channelResponse_applicationId :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Text)
channelResponse_applicationId = Lens.lens (\ChannelResponse' {applicationId} -> applicationId) (\s@ChannelResponse' {} a -> s {applicationId = a} :: ChannelResponse)

-- | The current version of the channel.
channelResponse_version :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Int)
channelResponse_version = Lens.lens (\ChannelResponse' {version} -> version) (\s@ChannelResponse' {} a -> s {version = a} :: ChannelResponse)

-- | (Deprecated) An identifier for the channel. This property is retained
-- only for backward compatibility.
channelResponse_id :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Text)
channelResponse_id = Lens.lens (\ChannelResponse' {id} -> id) (\s@ChannelResponse' {} a -> s {id = a} :: ChannelResponse)

-- | The date and time, in ISO 8601 format, when the channel was enabled.
channelResponse_creationDate :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Text)
channelResponse_creationDate = Lens.lens (\ChannelResponse' {creationDate} -> creationDate) (\s@ChannelResponse' {} a -> s {creationDate = a} :: ChannelResponse)

-- | The user who last modified the channel.
channelResponse_lastModifiedBy :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Text)
channelResponse_lastModifiedBy = Lens.lens (\ChannelResponse' {lastModifiedBy} -> lastModifiedBy) (\s@ChannelResponse' {} a -> s {lastModifiedBy = a} :: ChannelResponse)

-- | (Not used) This property is retained only for backward compatibility.
channelResponse_hasCredential :: Lens.Lens' ChannelResponse (Prelude.Maybe Prelude.Bool)
channelResponse_hasCredential = Lens.lens (\ChannelResponse' {hasCredential} -> hasCredential) (\s@ChannelResponse' {} a -> s {hasCredential = a} :: ChannelResponse)

instance Core.FromJSON ChannelResponse where
  parseJSON =
    Core.withObject
      "ChannelResponse"
      ( \x ->
          ChannelResponse'
            Prelude.<$> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "IsArchived")
            Prelude.<*> (x Core..:? "ApplicationId")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "HasCredential")
      )

instance Prelude.Hashable ChannelResponse where
  hashWithSalt salt' ChannelResponse' {..} =
    salt' `Prelude.hashWithSalt` hasCredential
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` isArchived
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` lastModifiedDate

instance Prelude.NFData ChannelResponse where
  rnf ChannelResponse' {..} =
    Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf hasCredential
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf isArchived
      `Prelude.seq` Prelude.rnf enabled
