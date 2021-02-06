import discord
import os
import configparser

config = configparser.ConfigParser()
with open("bot.cfg") as stream:
    config.read_string("[top]\n" + stream.read()) # https://stackoverflow.com/questions/2885190/using-configparser-to-read-a-file-without-section-name

VCTEXT_CHANNEL_ID = int(config['top']['vctext-channel'])
VCTEXT_ROLE_ID = discord.Object(int(config['top']['vctext-role']))
BOT_TOKEN = config['top']['bot-token']

client = discord.Client()

@client.event
async def on_ready():
    print('Python bot ready.')

@client.event
async def on_voice_state_update(member, before, after):
    print(before.channel and before.channel.id, after.channel and after.channel.id)
    print(VCTEXT_CHANNEL_ID)
    if (not before.channel or before.channel.id != VCTEXT_CHANNEL_ID) and \
        (after.channel and after.channel.id == VCTEXT_CHANNEL_ID):
        await member.add_roles(VCTEXT_ROLE_ID, reason='User joined Stream Voice Chat')
        print(f'Added the vctext role to {member.name}')

    if (before.channel and before.channel.id == VCTEXT_CHANNEL_ID) and \
        (not after.channel or after.channel.id != VCTEXT_CHANNEL_ID):
        await member.remove_roles(VCTEXT_ROLE_ID, reason='User left Stream Voice Chat')
        print(f'Removed the vctext role from {member.name}')




client.run(BOT_TOKEN)